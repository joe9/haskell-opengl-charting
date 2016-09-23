{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Main where

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
import GLFWHelpers
import OpenGLHelpers
import Scale
import Types
import Drawable
import Chart
import TestData
import Drawable.Screen
import Drawable.Frame
import Drawable.PriceAreaGraph
import Drawable.VolumeBarGraph
import Drawable.HorizontalCrosshair
import Drawable.VerticalCrosshair

-- fonts can be added using freetype2 or FontyFruity. edwardk
-- recommends using Valve approach of rendering it with a signed
-- distance field
-- nanovg uses Modern OpenGL to render and is a next-generation
-- gloss. It is a good idea to check it when things get sticky.
-- main :: IO ()
-- main = withGLFW $ window drawWindow
-- --   withGLFW $
-- --   do a <- asyncBound window
-- --      b <- asyncBound window
-- --      wait a
-- --      putStrLn "first window closed"
-- --      wait b
-- --      putStrLn "second window closed"
-- --      threadDelay (1 * 1000 * 1000)
main
    :: IO ()
main = do
    dataSeries <- buildDataSeries
    --     dataSeries <- return staticDataSeries
    let xscale = xScale dataSeries
        pricescale = priceScale dataSeries
        volumescale = volumeScale dataSeries
    putStrLn ("dataSeries: " ++ show dataSeries)
    --      putStrLn ("xScale: " ++ show xscale)
    --      putStrLn ("pricescale: " ++ show pricescale)
    --      putStrLn ("volumeScale: " ++ show volumescale)
    ref <- newIORef (dataSeries, xscale, pricescale, volumescale)
    a <-
        async
            (threadDelay (1 * 1000 * 1000) >>
             updatedData ref (dataSeries, xscale, pricescale, volumescale))
    --     withGLFW
    --         (window debugUsingSingleBuffer debugRenderer)
    withGLFW
        (window onWindow (renderDrawables ref))
    cancel a

onWindow :: ([Drawable] -> IO ()) -> IO ()
onWindow f = initializeDrawables f

-- http://stackoverflow.com/questions/5293898/how-to-pass-state-between-event-handlers-in-gtk2hs
-- the below is not a working solution. Use MVar or TVar or IORef as
-- recommended in the SO answer above
updatedData
    :: IORef (VU.Vector PriceData, Scale, Scale, Scale)
    -> (VU.Vector PriceData, Scale, Scale, Scale)
    -> IO b
updatedData ref oldData = do
    newSeries <- addAnother oldData
    atomicModifyIORef'
        ref
        (\_ ->
              (newSeries, ()))
    GLFW.postEmptyEvent
    threadDelay (1 * 1000 * 1000)
    updatedData ref newSeries

addAnother
    :: (VU.Vector PriceData, Scale, Scale, Scale)
    -> IO (VU.Vector PriceData, Scale, Scale, Scale)
addAnother (series,xscale,pricescale,volumescale) = do
    b <- randomRIO (1, 2)
    a <- randomRIO (2, 3)
    v <- randomRIO (0, 1000000)
    asf <- fmap ((fromIntegral :: Integer -> AsOf) . round) getPOSIXTime
    return
        ( VU.snoc series (b, a, v, asf)
        , addToDomain xscale (fromIntegral (VU.length series))
        , addToDomain (addToDomain pricescale b) a
        , addToDomain volumescale v)
  where
    addToDomain s = (sAddToDomain s) s

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
