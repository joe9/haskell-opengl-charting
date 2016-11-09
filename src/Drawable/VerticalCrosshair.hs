{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Drawable.VerticalCrosshair where

import           Data.Colour.Names
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed  as VU
import           "gl" Graphics.GL
import           Protolude            hiding (State)

import Drawable
import GLFWHelpers
import OpenGLHelpers
import Scale
import Types

buildVerticalCrosshair
  :: State
  -> VU.Vector PriceData
  -> Scale
  -> Scale
  -> Scale
  -> Drawable
  -> IO (IO ())
-- (0,0) for the cursor position is the top left corner
buildVerticalCrosshair state _ _ _ _ d
  | 0 > stateCursorX state || 0 == stateWindowWidth state = return (return ())
  | otherwise = do
    let f = fromIntegral :: Int -> Double
        x = ((2 * stateCursorX state) / f (stateWindowWidth state)) - 1
        vertices = VS.fromList [realToFrac x, -1, realToFrac x, 1]
    loadBuffer (dBufferId d) vertices
    return (glDrawArrays GL_LINES 0 (div (fromIntegral (VS.length vertices)) 2))

verticalCrosshairDrawable :: VertexArrayId -> BufferId -> Drawable
verticalCrosshairDrawable vaId bId =
  Drawable
  { dDraw = return ()
  , dLoadBufferAndBuildDrawFunction = buildVerticalCrosshair
  , dPreviousValue = Nothing
  , dCurrentValue =
      \s _ -> (ValueCursorPosition (stateCursorX s) (stateCursorY s))
  , dVertexArrayId = vaId
  , dBufferId = bId
  , dColour = green
  , dTransparency = Just 0.5
  , dType = VerticalCrosshair
  }
