{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Drawable where

--   https://ghc.haskell.org/trac/ghc/wiki/Commentary/Packages/PackageImportsProposal
import Control.Concurrent
import Control.Concurrent.Async
import Data.Bits
import Data.Colour.Names
import Data.IORef
import Data.Time.Clock.POSIX
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import "gl" Graphics.GL
import Graphics.UI.GLFW as GLFW
import Prelude hiding (init)
import System.Random
--
import Scale
import Types

data Value
    = ValueCursorPosition Double
                          Double
    | ValueDimensions Width
                      Height
    | ValueInt Int
    | ValueAsOf Int64
    | ValueInteger Integer
    | ValueEmpty
    deriving Eq

data DrawableType
    = Screen
    | Frame
    | PriceChart
    | VolumeChart
    | HorizontalCrosshair
    | VerticalCrosshair
    deriving Show

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
initializeDrawables
    :: ([Drawable] -> IO b) -> IO b
initializeDrawables continueFunction =
    withVertexArray $
    \svaid svabid -> do
        withVertexArray $
            \fvaid fvabid -> do
                withVertexArray $
                    \pvaid pvabid -> do
                        withVertexArray $
                            \vvaid vvabid -> do
                                withVertexArray $
                                    \hcvaid hcvabid -> do
                                        withVertexArray $
                                            \vcvaid vcvabid -> do
                                                continueFunction
                                                    [ screenDrawable
                                                          svaid
                                                          svabid
                                                    , frameDrawable
                                                          fvaid
                                                          fvabid
                                                    , priceChartDrawable
                                                          pvaid
                                                          pvabid
                                                    , volumeChartDrawable
                                                          vvaid
                                                          vvabid
                                                    , horizontalCrosshairDrawable
                                                          hcvaid
                                                          hcvabid
                                                    , verticalCrosshairDrawable
                                                          vcvaid
                                                          vcvabid]

debugUsingSingleBuffer
    :: ((VertexArrayId, BufferId, VertexArrayId, BufferId, VertexArrayId, BufferId) -> IO a)
    -> IO a
-- debugUsingSingleBuffer f = withVertexArray (\vaId -> do f vaId)
debugUsingSingleBuffer f =
    withVertexArray $
    \fvaid fbid ->
         withVertexArray $
         \svaid sbid ->
              withVertexArray $
              \_ _ ->
                   withVertexArray $
                   \_ _ ->
                        withVertexArray $
                        \_ _ ->
                             withVertexArray $
                             \vaId bId ->
                                  f (fvaid, fbid, svaid, sbid, vaId, bId)

debugRenderer
    :: Window
    -> ColorUniformLocation
    -> State
    -> (VertexArrayId, BufferId, VertexArrayId, BufferId, VertexArrayId, BufferId)
    -> IO (VertexArrayId, BufferId, VertexArrayId, BufferId, VertexArrayId, BufferId)
debugRenderer win colorUniformLocation state (fvaid,fbid,svaid,sbid,lvaid,lbid) = do
    let vertices =
            (VS.fromList
                 [ -1
                 , (0 :: GLfloat)
                 , -0.5
                 , 1
                 , 0
                 , 0
                 , 0
                 , 0
                 , 0.5
                 , 1
                 , 1
                 , 0
                 , 0.5
                 , 0
                 , 1
                 , -1
                 , 0
                 , -1])
    putStrLn ("Vertex Array Id: " ++ show svaid)
    justDrawThis
        win
        colorUniformLocation
        vertices
        (div (VS.length vertices) 2)
        GL_TRIANGLES
        1
        0
        0
        1
        svaid
        sbid
    threadDelay (2 * 1000 * 1000)
    let d = screenDrawable svaid sbid
    --     (dDraw s) -- this works
    drawUsingVertexArray
        win
        colorUniformLocation
        (dVertexArrayId d)
        (dColour d)
        (dTransparency d)
        (dDraw d)
    let f = frameDrawable fvaid fbid
    loadBuffer (dBufferId f) vertices
    drawUsingVertexArray
        win
        colorUniformLocation
        (dVertexArrayId f)
        (dColour f)
        (dTransparency f)
        (glDrawArrays GL_LINES 0 8)
    --     let f = frameDrawable vaId
    --     loadUsingVertexArray (dVertexArrayId f) vertices
    --     drawUsingVertexArray
    --         win
    --         colorUniformLocation
    --         (dVertexArrayId f)
    --         (dColour f)
    --         (dTransparency f)
    --         (glDrawArrays GL_LINES 0 8)
    -- below works too
    --     glClearColor 0.05 0.05 0.05 1
    --     glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
    --     glDrawArrays GL_LINES 0 8
    GLFW.swapBuffers
        win
    glFlush  -- not necessary, but someone recommended it
    threadDelay (2 * 1000 * 1000)
    return (fvaid, fbid, svaid, sbid, lvaid, lbid)