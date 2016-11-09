{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Drawable.HorizontalCrosshair where

import           Data.Colour.Names
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed  as VU
import           "gl" Graphics.GL
import           Protolude hiding (State)

import Drawable
import GLFWHelpers
import OpenGLHelpers
import Scale
import Types

-- (0,0) for the cursor position is the top left corner
buildHorizontalCrosshair
  :: State
  -> VU.Vector PriceData
  -> Scale
  -> Scale
  -> Scale
  -> Drawable
  -> IO (IO ())
buildHorizontalCrosshair state _ _ _ _ d
  | 0 > stateCursorY state || 0 == stateWindowHeight state = return (return ())
  | otherwise = do
    let f = fromIntegral :: Int -> Double
        ny = fromIntegral (stateWindowHeight state) - stateCursorY state
        y = (2 * ny / f (stateWindowHeight state)) - 1
        vertices = VS.fromList [-1, realToFrac y, 1, realToFrac y]
    loadBuffer (dBufferId d) vertices
    return (glDrawArrays GL_LINES 0 (div (fromIntegral (VS.length vertices)) 2))

horizontalCrosshairDrawable :: VertexArrayId -> BufferId -> Drawable
horizontalCrosshairDrawable vaId bId =
  Drawable
  { dDraw = return ()
  , dLoadBufferAndBuildDrawFunction = buildHorizontalCrosshair
  , dPreviousValue = Nothing
  , dCurrentValue =
      (\s _ -> (ValueCursorPosition (stateCursorX s) (stateCursorY s)))
  , dVertexArrayId = vaId
  , dBufferId = bId
  , dColour = green
  , dTransparency = Just 0.5
  , dType = HorizontalCrosshair
  }
