{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Main where

--   https://ghc.haskell.org/trac/ghc/wiki/Commentary/Packages/PackageImportsProposal
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM   (TQueue, atomically,
                                           newTQueueIO, tryReadTQueue,
                                           writeTQueue)
import           Control.Monad            (unless, void, when)
import           Data.Bits
import           Data.Colour.Names
import           Data.IORef
import           Data.Time.Clock.POSIX
import qualified Data.Vector.Storable     as VS
import qualified Data.Vector.Unboxed      as VU
import           "gl" Graphics.GL
import           Graphics.UI.GLFW         as GLFW
import           Protolude                hiding (State)
import           System.Random

import Chart
import Drawable
import Drawable.Frame
import Drawable.HorizontalCrosshair
import Drawable.PriceAreaGraph
import Drawable.Screen
import Drawable.VerticalCrosshair
import Drawable.VolumeBarGraph
import GLFWHelpers
import OpenGLHelpers
import Scale
import TestData
import PriceData

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
-- --      putText "first window closed"
-- --      wait b
-- --      putText "second window closed"
-- --      threadDelay (1 * 1000 * 1000)
main :: IO ()
main = do
  dataSeries <- buildDataSeries
  --     dataSeries <- return staticDataSeries
  let xscale = xScale dataSeries
      pricescale = priceScale dataSeries
      volumescale = volumeScale dataSeries
  putText ("dataSeries: " <> show dataSeries)
  --      putText ("xScale: " <> show xscale)
  --      putText ("pricescale: " <> show pricescale)
  --      putText ("volumeScale: " <> show volumescale)
  ref <- newIORef (dataSeries, xscale, pricescale, volumescale)
  a <-
    async
      (threadDelay (1 * 1000 * 1000) >>
       updatedData ref (dataSeries, xscale, pricescale, volumescale))
  -- check TestDrawable for simple functions to debug this
  withGLFW
    (withInitializedWindow
       (\e s ->
          withInitializedDrawables
            [ screenDrawable
            , frameDrawable
            , priceChartDrawable
            , volumeChartDrawable
            , horizontalCrosshairDrawable
            , verticalCrosshairDrawable
            ]
            (run ref e s)))
  cancel a

--   withGLFW
--     (withInitializedWindow (\e s -> debugUsingSingleBuffer (debugRenderer e s)))
run
  :: IORef (HashMap.HashMap AsOf PriceData, Scale, Scale, Scale)
  -> Env
  -> State
  -> [Drawable]
  -> IO ()
run ref env state ds
                  -- number of seconds since GLFW started
                  -- TODO bug: on empty event, should updated the chart with new data
 = do
  GLFW.waitEvents
  putText "Received GLFW event: "
  ustate <- processEvents env state
  q <- GLFW.windowShouldClose (envWindow env)
  unless
    q
    (do uuds <-
          (do dataSeries <- readIORef ref
              uds <- renderDrawables env ustate ds dataSeries
              return uds)
        run ref env ustate uuds)

--     previousmt <- liftIO GLFW.getTime
--     liftIO (GLFW.pollEvents)
processEvents :: Env -> State -> IO State
processEvents env state = do
  me <- atomically $ tryReadTQueue (envEventsChan env)
  case me of
    Just e -> do
      ustate <- processEvent env state e
      processEvents env ustate
    Nothing -> return state

-- http://stackoverflow.com/questions/5293898/how-to-pass-state-between-event-handlers-in-gtk2hs
-- the below is not a working solution. Use MVar or TVar or IORef as
-- recommended in the SO answer above
updatedData
  :: IORef (HashMap.HashMap AsOf PriceData, Scale, Scale, Scale)
  -> (HashMap.HashMap AsOf PriceData, Scale, Scale, Scale)
  -> IO b
updatedData ref oldData = do
  newSeries <- addAnother oldData
  atomicModifyIORef' ref (\_ -> (newSeries, ()))
  GLFW.postEmptyEvent
  threadDelay (1 * 1000 * 1000)
  updatedData ref newSeries

addAnother
  :: (HashMap.HashMap AsOf PriceData, Scale, Scale, Scale)
  -> IO (HashMap.HashMap AsOf PriceData, Scale, Scale, Scale)
addAnother (series, xscale, pricescale, volumescale) = do
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
