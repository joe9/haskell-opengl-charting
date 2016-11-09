{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE PackageImports #-}

module Drawable.VolumeBarGraph
  (volumeChartDrawable)
  where

import Protolude
import Data.Colour.Names
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import "gl" Graphics.GL
import Linear.V2
--
import OpenGLHelpers
import Scale
import Types
import Drawable

-- TODO            map dot scaledVolumes)
volumeBufferData
    :: Scale -> Scale -> VU.Vector PriceData -> VS.Vector Float
volumeBufferData xScale yScale dataSeries =
    ((VS.concatMap v2ToVertex .
      VU.convert .
      VU.concatMap
          (\(V2 x y) ->
                VU.fromList
                    [ V2 (x - barWidthHalved) (sMinRange yScale)
                    , V2 (x - barWidthHalved) y
                    , V2 (x + barWidthHalved) (sMinRange yScale)
                    , V2 (x + barWidthHalved) y]) .
      VU.imap (scaledVertex xScale yScale))
         dataSeries)
  where
    chartWidth = sMaxRange xScale - sMinRange xScale
    barWidthHalved = (barWidth chartWidth (VU.length dataSeries)) / 2

type NumberOfEntries = Int

barWidth :: Double -> NumberOfEntries -> Double
barWidth chartWidth n = chartWidth / (fromIntegral n)

-- Scale from the domain (input data range) to the range (absolute coordinate).
scaledVertex
    :: Scale -> Scale -> Int -> PriceData -> (V2 Double)
scaledVertex xScale yScale x =
    V2 (((sToRange xScale) xScale . fromIntegral) x) .
    (sToRange yScale) yScale . volume

volumeChartDrawable :: VertexArrayId -> BufferId -> Drawable
volumeChartDrawable vaId bId =
    Drawable
    { dDraw = return ()
    , dLoadBufferAndBuildDrawFunction = \_ dataSeries scalex _ scalevolume d -> do
          do let vertices = volumeBufferData scalex scalevolume dataSeries
             loadBuffer (dBufferId d) vertices
             return
                 (glDrawArrays
                      GL_TRIANGLE_STRIP
                      0
                      (div (fromIntegral (VS.length vertices)) 2))
    , dPreviousValue = Nothing
    , dCurrentValue = \_ ->
                           ValueAsOf . asof . VU.last
    , dVertexArrayId = vaId
    , dBufferId = bId
    , dColour = lightgrey
    , dTransparency = Nothing
    , dType = VolumeChart
    }
