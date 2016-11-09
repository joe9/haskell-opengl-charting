{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Chart where

import           Data.Colour.Names
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Vector.Storable as VS
import           "gl" Graphics.GL
import           Protolude            hiding (ask)

import Drawable
import GLFWHelpers
import OpenGLHelpers
import Scale
import TestData
import PriceData

minimumElement, maximumElement
  :: (Ord a, VU.Unbox b)
  => (b -> a) -> VU.Vector b -> a
minimumElement f = f . VU.minimumBy (\a b -> compare (f a) (f b))

maximumElement f = f . VU.maximumBy (\a b -> compare (f a) (f b))

xScale, priceScale, volumeScale :: HashMap.HashMap AsOf PriceData -> Scale
xScale dataSeries =
  linearScale
    ((minimum . HashMap.elems . HashMap.map pAsOf) dataSeries)
    ((maximum . HashMap.elems . HashMap.map pAsOf) dataSeries)
    (-1 + margin)
    (1 - margin)

priceScale dataSeries =
  linearScale
    (min ((minimum . HashMap.elems . HashMap.map pBid) dataSeries)
          (minimum . HashMap.elems . HashMap.map pAsk) dataSeries)
    (max ((maximum . HashMap.elems . HashMap.map pBid) dataSeries)
          (maximum . HashMap.elems . HashMap.map pAsk) dataSeries)
    (-1 + margin + volumeChartHeight 2)
    (-1 + margin + volumeChartHeight 2 + priceChartHeight 2)

volumeScale dataSeries =
  linearScale
    (minimumElement volume dataSeries)
    (maximumElement volume dataSeries)
    (-1 + margin)
    (-1 + margin + volumeChartHeight 2)

-- chart :: (Scale xscale
--          ,Scale priceScale
--          ,Scale volumeScale)
--       => xscale
--       -> priceScale
--       -> volumeScale
--       -> HashMap.HashMap AsOf PriceData
--       -> [Picture]
-- chart x p v dataSeries =
--   [ -- frame
-- --    pChart x p dataSeries
--    vChart x v dataSeries
--   ,horizontalCrosshair 0.5
--   ,verticalCrosshair 0.25]
-- The size of the chart, in logical units. All the diagrams use the
--  logical units. The translation from the actual units to the logical
--  units is done by the renderer. 100 corresponds to 100%.
margin :: Double
margin = 0.05

chartWidth, chartHeight, priceChartHeight, volumeChartHeight :: Double -> Double
chartWidth w = w - (2 * margin)

chartHeight h = h - (2 * margin)

priceChartHeight = (* 0.8) . chartHeight

volumeChartHeight = (* 0.2) . chartHeight
