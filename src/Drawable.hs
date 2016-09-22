{-# LANGUAGE PackageImports #-}

module Drawable where

--   https://ghc.haskell.org/trac/ghc/wiki/Commentary/Packages/PackageImportsProposal
import Data.Colour
import Data.IORef
import qualified Data.Vector.Unboxed as VU
import "gl" Graphics.GL
import Graphics.UI.GLFW as GLFW
import Prelude hiding (init)
import Data.Int
--
import Scale
import Types
import GLFWHelpers
import OpenGLHelpers

data Value
    = ValueCursorPosition Double
                          Double
    | ValueDimensions Width
                      Height
    | ValueInt Int
    | ValueAsOf Int64
    | ValueInteger Integer
    | ValueEmpty
    deriving (Eq)

data DrawableType
    = Screen
    | Frame
    | PriceChart
    | VolumeChart
    | HorizontalCrosshair
    | VerticalCrosshair
    deriving (Show)

data Drawable = Drawable
    { dPreviousValue :: Maybe Value
    , dCurrentValue :: State -> VU.Vector PriceData -> Value
    , dLoadBufferAndBuildDrawFunction :: State -> VU.Vector PriceData -> Scale -> Scale -> Scale -> Drawable -> IO (IO ())
    , dDraw :: IO ()
    , dVertexArrayId :: VertexArrayId
    , dBufferId :: BufferId
    , dColour :: Colour Double
    , dTransparency :: Maybe Double
    , dType :: DrawableType
    }

renderDrawables
    :: IORef (VU.Vector PriceData, Scale, Scale, Scale)
    -> Window
    -> ColorUniformLocation
    -> State
    -> [Drawable]
    -> IO [Drawable]
renderDrawables ref win colorUniformLocation state ds = do
    putStrLn "renderDrawables called"
    (series,_,_,_) <- readIORef ref
    if (any
            (\d ->
                  Just (dCurrentValue d state series) /= dPreviousValue d)
            ds)
        then do
            newds <-
                mapM (renderDrawable ref win colorUniformLocation state) ds
            GLFW.swapBuffers win
            glFlush  -- not necessary, but someone recommended it
            return newds
        else return ds

renderDrawable
    :: IORef (VU.Vector PriceData, Scale, Scale, Scale)
    -> Window
    -> ColorUniformLocation
    -> State
    -> Drawable
    -> IO Drawable
renderDrawable ref win colorUniformLocation state drawable = do
    let justDraw =
            (\d -> do
                 drawUsingVertexArray
                     win
                     colorUniformLocation
                     (dVertexArrayId d)
                     (dColour d)
                     (dTransparency d)
                     (dDraw d)
                 return d)
    (series,xscale,pricescale,volumescale) <- readIORef ref
    let newValue = dCurrentValue drawable state series
    if (Just newValue /= dPreviousValue drawable)
        then do
            putStrLn
                ("renderDrawable called - loading buffer and drawing of " ++
                 show (dType drawable))
            -- With OpenGL, the coordinates should be in the range (-1, 1)
            drawFunction <-
                (dLoadBufferAndBuildDrawFunction drawable)
                    state
                    series
                    xscale
                    pricescale
                    volumescale
                    drawable
            justDraw
                (drawable
                 { dDraw = drawFunction
                 , dPreviousValue = Just newValue
                 })
        else do
            putStrLn
                ("renderDrawable called - drawing " ++ show (dType drawable))
            justDraw drawable

-- could use the ContT monad. but, this is more readable
--  https://github.com/glguy/irc-core/blob/v2/src/Client/CApi.hs#L146-L158
-- initializeDrawables
--     :: ([Drawable] -> IO b) -> IO b
-- initializeDrawables continueFunction =
--     withVertexArray $
--     \svaid svabid -> do
--         withVertexArray $
--             \fvaid fvabid -> do
--                 withVertexArray $
--                     \pvaid pvabid -> do
--                         withVertexArray $
--                             \vvaid vvabid -> do
--                                 withVertexArray $
--                                     \hcvaid hcvabid -> do
--                                         withVertexArray $
--                                             \vcvaid vcvabid -> do
--                                                 continueFunction
--                                                     [ screenDrawable
--                                                           svaid
--                                                           svabid
--                                                     , frameDrawable
--                                                           fvaid
--                                                           fvabid
--                                                     , priceChartDrawable
--                                                           pvaid
--                                                           pvabid
--                                                     , volumeChartDrawable
--                                                           vvaid
--                                                           vvabid
--                                                     , horizontalCrosshairDrawable
--                                                           hcvaid
--                                                           hcvabid
--                                                     , verticalCrosshairDrawable
--                                                           vcvaid
--                                                           vcvabid]
