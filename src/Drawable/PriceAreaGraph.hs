{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Drawable.PriceAreaGraph
  ( priceChartDrawable
  ) where

import           Data.Colour.Names
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Vector.Storable as VS
import           "gl" Graphics.GL
import           Protolude            hiding (ask)

import Drawable
import OpenGLHelpers
import Scale
import PriceData

priceChartDrawable :: VertexArrayId -> BufferId -> Drawable
priceChartDrawable vaId bId =
  Drawable
  { dDraw = return ()
  , dLoadBufferAndBuildDrawFunction =
      \_ dataSeries scalex scaleprice _ d -> do
        let vertices = priceBufferData scalex scaleprice dataSeries
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
  , dColour = lightpink
  , dTransparency = Nothing
  , dType = PriceChart
  }

-- TODO dots
--   Pictures ([areaBetweenBidAndAsk areaVertices] <> map dot scaledBids <>
--             map dot scaledAsks)
priceGraph :: Scale -> Scale -> HashMap.HashMap AsOf PriceData -> Picture
priceGraph scalex scaley dataSeries =
  Picture
    (priceBufferData scalex scaley dataSeries)
    GL_TRIANGLE_STRIP
    lightpink
    Nothing

priceBufferData :: Scale
                -> Scale
                -> HashMap.HashMap AsOf PriceData
                -> VS.Vector Float
priceBufferData scalex scaley =
  VS.fromList . concatMap (scaledPrice scalex scaley) . toSortedList

-- scaledPriceOld
--   :: (Scale x
--      ,Scale y)
--   => x -> y -> (Int,PriceData) -> VU.Vector (V2 Double)
-- scaledPriceOld xScale yScale (x,d) =
--   VU.cons (V2 ((toRange xScale . fromIntegral) x)
--               ((toRange yScale) b))
--           (VU.singleton
--              (V2 ((toRange xScale . fromIntegral) x)
--                  ((toRange yScale) a)))
--   where b = bid d
--         a = ask d
--   where scaledPrices = (VS.concatMap v2ToVertex . VU.convert . VU.concatMap (scaledPriceOld xScale yScale) . VU.indexed) dataSeries
-- Scale from the domain (input data range) to the range (absolute coordinate).
scaledPrice :: Scale -> Scale -> (AsOf, PriceData) -> [Float]
scaledPrice scalex scaley (x, d) =
  [ (realToFrac . sToRange scalex scalex . fromIntegral) x
  , (realToFrac . (sToRange scaley) scaley) b
  , (realToFrac . sToRange scalex scalex . fromIntegral) x
  , (realToFrac . (sToRange scaley) scaley) a
  ]
  where
    b = bid d
    a = ask d
