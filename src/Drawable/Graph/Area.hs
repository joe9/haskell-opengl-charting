{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Drawable.Graph.Area
  ( areaDrawable
  ) where

import           Data.Colour.Names
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed  as VU
import           "gl" Graphics.GL
import           Protolude

--
import Drawable
import GLFWHelpers
import OpenGLHelpers
import Scale
import Types

-- TODO : could not complete this. Will have to do this to make the
-- module more generic
-- for priceDrawable: fx = fst, fy1 = bid, fy2 = ask
loadAndDrawFunction
  :: ((Int, PriceData) -> Double)
  -> ((Int, PriceData) -> Double)
  -> ((Int, PriceData) -> Double)
  -> (State -> HashMap.HashMap AsOf PriceData -> Scale -> Scale -> Scale -> Drawable -> IO (IO ()))
loadAndDrawFunction fx fy1 fy2 =
  \_ dataSeries scalex scaleprice _ d -> do
    let vertices = priceBufferData scalex (scaleprice fx fy1 fy2) dataSeries
    loadBuffer (dBufferId d) vertices
    return
      (glDrawArrays
         GL_TRIANGLE_STRIP
         0
         (div (fromIntegral (VS.length vertices)) 2))

areaDrawable :: VertexArrayId -> BufferId -> Drawable
areaDrawable vaId bId =
  Drawable
  { dDraw = return ()
  , dLoadBufferAndBuildDrawFunction = loadAndDrawFunction
  , dPreviousValue = Nothing
  , dCurrentValue = \_ -> ValueAsOf . asof . VU.last
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
  Picture scaledPrices GL_TRIANGLE_STRIP lightpink Nothing
  where
    scaledPrices =
      (VU.convert . VU.concatMap (scaledPrice scalex scaley) . VU.indexed)
        dataSeries

priceBufferData :: Scale -> Scale -> HashMap.HashMap AsOf PriceData -> VS.Vector Float
priceBufferData scalex scaley =
  VU.convert . VU.concatMap (scaledPrice scalex scaley) . VU.indexed

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
scaledPrice
  :: ((Int, PriceData) -> Double)
  -> ((Int, PriceData) -> Double)
  -> ((Int, PriceData) -> Double)
  -> Scale
  -> Scale
  -> (Int, PriceData)
  -> VU.Vector Float
scaledPrice fx fy1 fy2 scalex scaley d =
  VU.fromList
    [ (realToFrac . sToRange scalex scalex . fromIntegral) x
    , (realToFrac . (sToRange scaley) scaley) b
    , (realToFrac . sToRange scalex scalex . fromIntegral) x
    , (realToFrac . (sToRange scaley) scaley) a
    ]
  where
    x = fx d
    b = fy1 d
    a = fy2 d
