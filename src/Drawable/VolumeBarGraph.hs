{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Drawable.VolumeBarGraph
  ( volumeChartDrawable
  ) where

import           Data.Colour.Names
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Vector.Storable as VS
import           "gl" Graphics.GL
import           Linear.V2
import           Protolude

import Drawable
import OpenGLHelpers
import Scale
import PriceData

-- TODO            map dot scaledVolumes)
volumeBufferData :: Scale
                 -> Scale
                 -> HashMap.HashMap AsOf PriceData
                 -> VS.Vector Float
volumeBufferData xScale yScale dataSeries =
  (VS.fromList .
   concatMap
     (\(x, y) ->
        [ realToFrac (x - barWidthHalved)
        , realToFrac (sMinRange yScale)
        , realToFrac (x - barWidthHalved)
        , realToFrac y
        , realToFrac (x + barWidthHalved)
        , realToFrac (sMinRange yScale)
        , realToFrac (x + barWidthHalved)
        , realToFrac y
        ]) .
   fmap (scaledVertex xScale yScale) . toSortedList)
    dataSeries
  where
    chartWidth = sMaxRange xScale - sMinRange xScale
    barWidthHalved = (barWidth chartWidth (HashMap.size dataSeries)) / 2

type NumberOfEntries = Int

barWidth :: Double -> NumberOfEntries -> Double
barWidth chartWidth n = chartWidth / (fromIntegral n)

-- Scale from the domain (input data range) to the range (absolute coordinate).
scaledVertex :: Scale -> Scale -> (AsOf, PriceData) -> (Double, Double)
scaledVertex xScale yScale (x, p) =
  ( ((sToRange xScale) xScale . fromIntegral) x
  , ((sToRange yScale) yScale . volume) p)

volumeChartDrawable :: VertexArrayId -> BufferId -> Drawable
volumeChartDrawable vaId bId =
  Drawable
  { dDraw = return ()
  , dLoadBufferAndBuildDrawFunction =
      \_ dataSeries scalex _ scalevolume d -> do
        do let vertices = volumeBufferData scalex scalevolume dataSeries
           loadBuffer (dBufferId d) vertices
           return
             (glDrawArrays
                GL_TRIANGLE_STRIP
                0
                (div (fromIntegral (VS.length vertices)) 2))
  , dPreviousValue = Nothing
  , dCurrentValue = \_ -> ValueAsOf . latestAsOf
  , dVertexArrayId = vaId
  , dBufferId = bId
  , dColour = lightgrey
  , dTransparency = Nothing
  , dType = VolumeChart
  }
