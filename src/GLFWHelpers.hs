{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module GLFWHelpers where

--------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Concurrent.STM    (TQueue, atomically,
                                            newTQueueIO,
                                            tryReadTQueue,
                                            writeTQueue)
import           Control.Exception.Safe
import           Control.Monad             (unless, void, when)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Data.Bits
import           Data.Maybe
import           Data.String.Conversions   (cs)
import           Data.Text                 (unwords)
import           Data.Text.IO
import           "gl" Graphics.GL
import           Protolude                 hiding (State, bracket)
import           Text.PrettyPrint          hiding ((<>))
import qualified Text.PrettyPrint          as PP

-- import Quine.GL.Version
import qualified Graphics.UI.GLFW as GLFW

import GLException
import OpenGLHelpers hiding (rgb)

--------------------------------------------------------------------------------
data Env = Env
  { envEventsChan           :: TQueue Event
  , envWindow               :: !GLFW.Window
  , envColorUniformLocation :: GLint
  }

data State = State
  { stateWindowWidth  :: !Int
  , stateWindowHeight :: !Int
  , stateMouseDown    :: !Bool
  , stateDragging     :: !Bool
  , stateDragStartX   :: !Double
  , stateDragStartY   :: !Double
  , stateCursorX      :: !Double
  , stateCursorY      :: !Double
  } deriving (Show)

--------------------------------------------------------------------------------
data Event
  = EventError !GLFW.Error
               !Text
  | EventWindowPos !GLFW.Window
                   !Int
                   !Int
  | EventWindowSize !GLFW.Window
                    !Int
                    !Int
  | EventWindowClose !GLFW.Window
  | EventWindowRefresh !GLFW.Window
  | EventWindowFocus !GLFW.Window
                     !GLFW.FocusState
  | EventWindowIconify !GLFW.Window
                       !GLFW.IconifyState
  | EventFramebufferSize !GLFW.Window
                         !Int
                         !Int
  | EventMouseButton !GLFW.Window
                     !GLFW.MouseButton
                     !GLFW.MouseButtonState
                     !GLFW.ModifierKeys
  | EventCursorPos !GLFW.Window
                   !Double
                   !Double
  | EventCursorEnter !GLFW.Window
                     !GLFW.CursorState
  | EventScroll !GLFW.Window
                !Double
                !Double
  | EventKey !GLFW.Window
             !GLFW.Key
             !Int
             !GLFW.KeyState
             !GLFW.ModifierKeys
  | EventChar !GLFW.Window
              !Char
  | EventCharMods !GLFW.Window
                  !Char
                  !Int
  | EventMonitor !GLFW.Monitor
                 !GLFW.MonitorState
  deriving (Show) -- http://stackoverflow.com/questions/23844813/opengl-program-compiles-but-gives-error-when-run-ubuntu-14-04

-- use 3.3 context as my video card supports only that as reported by glxinfo
--------------------------------------------------------------------------------
withGLFW :: IO b -> IO b
withGLFW f
         -- turn simpleErrorcallback to an exception throwing function
         -- Callback functions must be set, so GLFW knows to call them. The
         -- function to set the error callback is one of the few GLFW
         -- functions that may be called before initialization, which lets
         -- you be notified of errors both during and after initialization.
 =
  GLFW.setErrorCallback (Just simpleErrorCallback) >>
  bracket
    GLFW.init
    (\_ -> GLFW.terminate >> GLFW.setErrorCallback (Just simpleErrorCallback))
    (\successfulInit ->
       if successfulInit
         then do
           GLFW.windowHint (GLFW.WindowHint'OpenGLDebugContext True)
           --       GLFW.windowHint $ GLFW.WindowHint'DepthBits 16
           GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
           GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
           GLFW.windowHint
             (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
           GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
           f
         else throw GLFWInitFailed)

withInitializedWindow :: (Env -> State -> IO ()) -> IO ()
withInitializedWindow continueFunction = do
  let width = 640
      height = 480
  eventsChan <- newTQueueIO :: IO (TQueue Event)
  withWindow width height "GLFW-b-demo" $ \win colorUniformLocation -> do
    setupCallbacks win eventsChan
    -- disable vsync (0 = off, 1 = on), 0 is the
    -- default value too
    -- http://www.glfw.org/docs/latest/quick.html#quick_swap_buffers
    GLFW.swapInterval 0
    (fbWidth, fbHeight) <- GLFW.getFramebufferSize win
    let env =
          Env
          { envEventsChan = eventsChan
          , envWindow = win
          , envColorUniformLocation = colorUniformLocation
          }
        state =
          State
          { stateWindowWidth = fbWidth
          , stateWindowHeight = fbHeight
          , stateMouseDown = False
          , stateDragging = False
          , stateDragStartX = 0
          , stateDragStartY = 0
          , stateCursorX = -1
          , stateCursorY = -1
          }
    printInstructions
    ustate <- adjustWindow env state
    continueFunction env ustate

--         GLFW.swapInterval 1
--------------------------------------------------------------------------------
-- GLFW-b is made to be very close to the C API, so creating a window is pretty
-- clunky by Haskell standards. A higher-level API would have some function
-- like withWindow.
withWindow :: Int
           -> Int
           -> Text
           -> (GLFW.Window -> ColorUniformLocation -> IO ())
           -> IO ()
withWindow width height title f = do
  bracket
    (GLFW.createWindow width height (cs title) Nothing Nothing)
    (\maybeWindow ->
       GLFW.setErrorCallback (Just simpleErrorCallback) >>
       maybe (return ()) GLFW.destroyWindow maybeWindow >>
       putText "ended window!")
    (maybe
       (return ())
       (\win -> do
          GLFW.makeContextCurrent (Just win)
          -- OpenGL stuff
          threadDelay 1000000
          major <- GLFW.getWindowContextVersionMajor win
          minor <- GLFW.getWindowContextVersionMinor win
          revision <- GLFW.getWindowContextVersionRevision win
          putText
            ("OpenGL version recieved: " <> show major <> "," <> show minor <>
             "," <>
             show revision)
          version <- GLFW.getVersion
          putText ("Supported GLFW Version is: " <> show version)
          versionString <- GLFW.getVersionString
          putText ("Supported GLFW Version String is: " <> show versionString)
          contextFlags <- getIntegerv GL_CONTEXT_FLAGS
          putText ("GL Context flags are: " <> show contextFlags)
          if contextFlags .&. GL_CONTEXT_FLAG_DEBUG_BIT == 2
            then putText "GL Debug Flag is set"
            else putText "GL Debug Flag is not set"
          -- OpenGLHelpers.withProgram call to installDebugHook installs glDebugMessagecallback
          -- check 4-arcan-eglintro.c or glfw/tests/glfwinfo.c for more details
          checkGLErrors (glEnable GL_DEBUG_OUTPUT)
          vendor <- checkGLErrors (getText GL_VENDOR)
          putText ("GL_VENDOR: " <> vendor)
          version <- checkGLErrors (getText GL_VERSION)
          putText ("GL_VENDOR: " <> version)
          renderer <- checkGLErrors (getText GL_RENDERER)
          putText ("GL_RENDERER: " <> renderer)
          --         putText ("GL_VERSION: " <> show version)
          --         putText ("Supported GLSL is %s\n", (const char*)glGetString(GL_SHADING_LANGUAGE_VERSION));
          glslversion <- checkGLErrors (getText GL_SHADING_LANGUAGE_VERSION)
          putText ("GL_SHADING_LANGUAGE_VERSION: " <> glslversion)
          withProgram
            (\programId -> colorUniformLocationInProgram programId >>= f win)))

-- Callback functions must be set, so GLFW knows to call them. The
-- function to set the error callback is one of the few GLFW functions
-- that may be called before initialization, which lets you be
-- notified of errors both during and after initialization.
simpleErrorCallback
  :: (Show a, Show a1)
  => a -> a1 -> IO ()
simpleErrorCallback e s = hPutStrLn stderr (unwords [show e, show s])

setupCallbacks :: GLFW.Window -> TQueue Event -> IO ()
setupCallbacks win eventsChan = do
  GLFW.setErrorCallback $ Just $ (\e s -> errorCallback eventsChan e (cs s))
  GLFW.setWindowPosCallback win $ Just $ windowPosCallback eventsChan
  GLFW.setWindowSizeCallback win $ Just $ windowSizeCallback eventsChan
  GLFW.setWindowCloseCallback win $ Just $ windowCloseCallback eventsChan
  GLFW.setWindowRefreshCallback win $ Just $ windowRefreshCallback eventsChan
  GLFW.setWindowFocusCallback win $ Just $ windowFocusCallback eventsChan
  GLFW.setWindowIconifyCallback win $ Just $ windowIconifyCallback eventsChan
  GLFW.setFramebufferSizeCallback win $
    Just $ framebufferSizeCallback eventsChan
  GLFW.setMouseButtonCallback win $ Just $ mouseButtonCallback eventsChan
  GLFW.setCursorPosCallback win $ Just $ cursorPosCallback eventsChan
  GLFW.setCursorEnterCallback win $ Just $ cursorEnterCallback eventsChan
  GLFW.setScrollCallback win $ Just $ scrollCallback eventsChan
  GLFW.setKeyCallback win $ Just $ keyCallback eventsChan
  GLFW.setCharCallback win $ Just $ charCallback eventsChan
  GLFW.setMonitorCallback $ Just $ monitorCallback eventsChan

--         GLFW.setCharModsCallback        win $ Just $ charModsCallback        eventsChan
--         GLFW.setDropCallback            win $ Just $ dropCallback            eventsChan
--------------------------------------------------------------------------------
-- Each callback does just one thing: write an appropriate Event to the events
-- TQueue.
errorCallback :: TQueue Event -> GLFW.Error -> Text -> IO ()
windowPosCallback :: TQueue Event -> GLFW.Window -> Int -> Int -> IO ()
windowSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int -> IO ()
windowCloseCallback :: TQueue Event -> GLFW.Window -> IO ()
windowRefreshCallback :: TQueue Event -> GLFW.Window -> IO ()
windowFocusCallback :: TQueue Event -> GLFW.Window -> GLFW.FocusState -> IO ()
windowIconifyCallback :: TQueue Event
                      -> GLFW.Window
                      -> GLFW.IconifyState
                      -> IO ()
framebufferSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int -> IO ()
mouseButtonCallback
  :: TQueue Event
  -> GLFW.Window
  -> GLFW.MouseButton
  -> GLFW.MouseButtonState
  -> GLFW.ModifierKeys
  -> IO ()
cursorPosCallback :: TQueue Event -> GLFW.Window -> Double -> Double -> IO ()
cursorEnterCallback :: TQueue Event -> GLFW.Window -> GLFW.CursorState -> IO ()
scrollCallback :: TQueue Event -> GLFW.Window -> Double -> Double -> IO ()
keyCallback
  :: TQueue Event
  -> GLFW.Window
  -> GLFW.Key
  -> Int
  -> GLFW.KeyState
  -> GLFW.ModifierKeys
  -> IO ()
charCallback :: TQueue Event -> GLFW.Window -> Char -> IO ()
charModsCallback :: TQueue Event -> GLFW.Window -> Char -> Int -> IO ()
-- dropCallback            :: TQueue Event -> GLFW.Window -> Char -> Int ->                                                      -> IO ()
monitorCallback :: TQueue Event -> GLFW.Monitor -> GLFW.MonitorState -> IO ()
-- TODO change this to throw an exception
errorCallback tc e s = atomically $ writeTQueue tc $ EventError e s

windowPosCallback tc win x y =
  atomically $ writeTQueue tc $ EventWindowPos win x y

windowSizeCallback tc win w h =
  atomically $ writeTQueue tc $ EventWindowSize win w h

windowCloseCallback tc win = atomically $ writeTQueue tc $ EventWindowClose win

windowRefreshCallback tc win =
  atomically $ writeTQueue tc $ EventWindowRefresh win

windowFocusCallback tc win fa =
  atomically $ writeTQueue tc $ EventWindowFocus win fa

windowIconifyCallback tc win ia =
  atomically $ writeTQueue tc $ EventWindowIconify win ia

framebufferSizeCallback tc win w h =
  atomically $ writeTQueue tc $ EventFramebufferSize win w h

mouseButtonCallback tc win mb mba mk =
  atomically $ writeTQueue tc $ EventMouseButton win mb mba mk

cursorPosCallback tc win x y =
  atomically $ writeTQueue tc $ EventCursorPos win x y

cursorEnterCallback tc win ca =
  atomically $ writeTQueue tc $ EventCursorEnter win ca

scrollCallback tc win x y = atomically $ writeTQueue tc $ EventScroll win x y

keyCallback tc win k sc ka mk =
  atomically $ writeTQueue tc $ EventKey win k sc ka mk

charCallback tc win c = atomically $ writeTQueue tc $ EventChar win c

charModsCallback tc win c sc =
  atomically $ writeTQueue tc $ EventCharMods win c sc

-- dropCallback
monitorCallback tc mon c = atomically $ writeTQueue tc $ EventMonitor mon c

--------------------------------------------------------------------------------
processEvent :: Env -> State -> Event -> IO State
processEvent env state ev =
  case ev
       -- this check on EventError is unneccessary. When an exception is thrown/raised,
       -- the caller should automatically destroy window (the caller
       -- should be using finally or bracket)
        of
    (EventError e s) -> do
      printEvent "error" [show e, show s]
      GLFW.setWindowShouldClose (envWindow env) True
      return state
    (EventWindowPos _ x y) ->
      printEvent "window pos" [show x, show y] >> return state
    (EventWindowSize _ width height) ->
      printEvent "window size" [show width, show height] >> return state
    (EventWindowClose _) -> printEvent "window close" [] >> return state
    (EventWindowRefresh _) -> printEvent "window refresh" [] >> return state
    (EventWindowFocus _ fs) ->
      printEvent "window focus" [show fs] >> return state
    (EventWindowIconify _ is) ->
      printEvent "window iconify" [show is] >> return state
    (EventFramebufferSize _ width height) -> do
      printEvent "framebuffer size" [show width, show height]
      (adjustWindow
         env
         state {stateWindowWidth = width, stateWindowHeight = height})
    (EventMouseButton _ mb mbs mk) -> do
      printEvent "mouse button" [show mb, show mbs, showModifierKeys mk]
      if (mb == GLFW.MouseButton'1)
        then let pressed = mbs == GLFW.MouseButtonState'Pressed
                 ustate = state {stateMouseDown = pressed}
             in if (not pressed)
                  then return (ustate {stateDragging = False})
                  else return ustate
        else return state
    (EventCursorPos _ x y) -> do
      let x' = round x :: Int
          y' = round y :: Int
      printEvent "cursor pos" [show x', show y']
      if (stateMouseDown state && not (stateDragging state))
        then return
               (state
                { stateDragging = True
                , stateDragStartX = x
                , stateDragStartY = y
                , stateCursorX = x
                , stateCursorY = y
                })
        else return (state {stateCursorX = x, stateCursorY = y})
    (EventCursorEnter _ cs) ->
      printEvent "cursor enter" [show cs] >> return state
    (EventScroll _ x y) -> do
      let x' = round x :: Int
          y' = round y :: Int
      printEvent "scroll" [show x', show y']
      adjustWindow env state
    (EventKey win k scancode ks mk) -> do
      printEvent "key" [show k, show scancode, show ks, showModifierKeys mk]
      when (ks == GLFW.KeyState'Pressed) $
      -- Q, Esc: exit
       do
        when (k == GLFW.Key'Q || k == GLFW.Key'Escape) $
          GLFW.setWindowShouldClose win True
        -- ?: print instructions
        when (k == GLFW.Key'Slash && GLFW.modifierKeysShift mk) $
          printInstructions
        -- i: print GLFW information
        when (k == GLFW.Key'I) $ printInformation win
      return state
    (EventChar _ c) -> printEvent "char" [show c] >> return state
    (EventCharMods _ c _) -> printEvent "char mods" [show c] >> return state
    (EventMonitor _ c) -> printEvent "monitor" [show c] >> return state

adjustWindow :: Env -> State -> IO State
adjustWindow _ state = do
  let width = stateWindowWidth state
      height = stateWindowHeight state
  glViewport 0 0 (fromIntegral width) (fromIntegral height)
  return state

getCursorKeyDirections :: GLFW.Window -> IO (Double, Double)
getCursorKeyDirections win = do
  x0 <- isPress `fmap` GLFW.getKey win GLFW.Key'Up
  x1 <- isPress `fmap` GLFW.getKey win GLFW.Key'Down
  y0 <- isPress `fmap` GLFW.getKey win GLFW.Key'Left
  y1 <- isPress `fmap` GLFW.getKey win GLFW.Key'Right
  let x0n =
        if x0
          then (-1)
          else 0
      x1n =
        if x1
          then 1
          else 0
      y0n =
        if y0
          then (-1)
          else 0
      y1n =
        if y1
          then 1
          else 0
  return (x0n + x1n, y0n + y1n)

getJoystickDirections :: GLFW.Joystick -> IO (Double, Double)
getJoystickDirections js = do
  maxes <- GLFW.getJoystickAxes js
  return $
    case maxes of
      (Just (x:y:_)) -> (-y, x)
      _              -> (0, 0)

isPress :: GLFW.KeyState -> Bool
isPress GLFW.KeyState'Pressed   = True
isPress GLFW.KeyState'Repeating = True
isPress _                       = False

--------------------------------------------------------------------------------
printInstructions :: IO ()
printInstructions =
  putText . cs $
  render $
  nest
    4
    (text "------------------------------------------------------------" $+$
     text "'?': Print these instructions" $+$
     text "'i': Print GLFW information" $+$
     text "" $+$
     text "* Mouse cursor, keyboard cursor keys, and/or joystick" $+$
     text "  control rotation." $+$
     text "* Mouse scroll wheel controls distance from scene." $+$
     text "------------------------------------------------------------")

printInformation :: GLFW.Window -> IO ()
printInformation win = do
  version <- GLFW.getVersion
  versionString <- GLFW.getVersionString
  monitorInfos <- runMaybeT getMonitorInfos
  joystickNames <- getJoystickNames
  clientAPI <- GLFW.getWindowClientAPI win
  cv0 <- GLFW.getWindowContextVersionMajor win
  cv1 <- GLFW.getWindowContextVersionMinor win
  cv2 <- GLFW.getWindowContextVersionRevision win
  robustness <- GLFW.getWindowContextRobustness win
  forwardCompat <- GLFW.getWindowOpenGLForwardCompat win
  debug <- GLFW.getWindowOpenGLDebugContext win
  profile <- GLFW.getWindowOpenGLProfile win
  putText . cs $
    render $
    nest
      4
      (text "------------------------------------------------------------" $+$
       text "GLFW C library:" $+$
       nest
         4
         (text "Version:" <+>
          renderVersion version $+$ text "Version string:" <+>
          renderVersionString versionString) $+$
       text "Monitors:" $+$
       nest 4 (renderMonitorInfos monitorInfos) $+$
       text "Joysticks:" $+$
       nest 4 (renderJoystickNames joystickNames) $+$
       text "OpenGL context:" $+$
       nest
         4
         (text "Client API:" <+>
          renderClientAPI clientAPI $+$ text "Version:" <+>
          renderContextVersion cv0 cv1 cv2 $+$ text "Robustness:" <+>
          renderContextRobustness robustness $+$ text "Forward compatibility:" <+>
          renderForwardCompat forwardCompat $+$ text "Debug:" <+>
          renderDebug debug $+$ text "Profile:" <+> renderProfile profile) $+$
       text "------------------------------------------------------------")
  where
    renderVersion (GLFW.Version v0 v1 v2) =
      text $ intercalate "." $ map show [v0, v1, v2]
    renderVersionString = text . show
    renderMonitorInfos = maybe (text "(error)") (vcat . map renderMonitorInfo)
    renderMonitorInfo (name, (x, y), (w, h), vms) =
      text (show name) $+$
      nest 4 (location <+> size $+$ fsep (map renderVideoMode vms))
      where
        location = int x <> text "," <> int y
        size = int w <> text "x" <> int h <> text "mm"
    renderVideoMode (GLFW.VideoMode w h r g b rr) =
      brackets $ res <+> rgb <+> hz
      where
        res = int w <> text "x" <> int h
        rgb = int r <> text "x" <> int g <> text "x" <> int b
        hz = int rr <> text "Hz"
    renderJoystickNames pairs =
      vcat $ map (\(js, name) -> text (show js) <+> text (show name)) pairs
    renderContextVersion v0 v1 v2 =
      hcat [int v0, text ".", int v1, text ".", int v2]
    renderClientAPI = text . show
    renderContextRobustness = text . show
    renderForwardCompat = text . show
    renderDebug = text . show
    renderProfile = text . show

type MonitorInfo = (Text, (Int, Int), (Int, Int), [GLFW.VideoMode])

getMonitorInfos :: MaybeT IO [MonitorInfo]
getMonitorInfos = getMonitors >>= mapM getMonitorInfo
  where
    getMonitors :: MaybeT IO [GLFW.Monitor]
    getMonitors = MaybeT GLFW.getMonitors
    getMonitorInfo :: GLFW.Monitor -> MaybeT IO MonitorInfo
    getMonitorInfo mon = do
      name <- getMonitorName mon
      vms <- getVideoModes mon
      MaybeT $ do
        pos <- GLFW.getMonitorPos mon
        size <- GLFW.getMonitorPhysicalSize mon
        return $ Just (name, pos, size, vms)
    getMonitorName :: GLFW.Monitor -> MaybeT IO Text
    getMonitorName mon = (MaybeT . fmap (fmap cs)) $ GLFW.getMonitorName mon
    getVideoModes :: GLFW.Monitor -> MaybeT IO [GLFW.VideoMode]
    getVideoModes mon = MaybeT $ GLFW.getVideoModes mon

getJoystickNames :: IO [(GLFW.Joystick, Text)]
getJoystickNames = catMaybes `fmap` mapM getJoystick joysticks
  where
    getJoystick js =
      fmap
        (maybe Nothing (\name -> Just (js, cs name)))
        (GLFW.getJoystickName js)

--------------------------------------------------------------------------------
printEvent :: Text -> [Text] -> IO ()
printEvent cbname fields = putText (cbname <> ": " <> unwords fields)

showModifierKeys :: GLFW.ModifierKeys -> Text
showModifierKeys mk = "[mod keys: " <> keys <> "]"
  where
    keys =
      if null xs
        then "none"
        else unwords xs
    xs = catMaybes ys
    ys =
      [ if GLFW.modifierKeysShift mk
          then Just "shift"
          else Nothing
      , if GLFW.modifierKeysControl mk
          then Just "control"
          else Nothing
      , if GLFW.modifierKeysAlt mk
          then Just "alt"
          else Nothing
      , if GLFW.modifierKeysSuper mk
          then Just "super"
          else Nothing
      ]

curb
  :: Ord a
  => a -> a -> a -> a
curb l h x
  | x < l = l
  | x > h = h
  | otherwise = x

--------------------------------------------------------------------------------
joysticks :: [GLFW.Joystick]
joysticks =
  [ GLFW.Joystick'1
  , GLFW.Joystick'2
  , GLFW.Joystick'3
  , GLFW.Joystick'4
  , GLFW.Joystick'5
  , GLFW.Joystick'6
  , GLFW.Joystick'7
  , GLFW.Joystick'8
  , GLFW.Joystick'9
  , GLFW.Joystick'10
  , GLFW.Joystick'11
  , GLFW.Joystick'12
  , GLFW.Joystick'13
  , GLFW.Joystick'14
  , GLFW.Joystick'15
  , GLFW.Joystick'16
  ]
