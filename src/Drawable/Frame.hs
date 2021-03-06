{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Drawable.Frame where

import           Data.Colour.Names
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Vector.Storable as VS
import           "gl" Graphics.GL
import           Protolude            hiding (State)

import Drawable
import GLFWHelpers
import OpenGLHelpers
import Scale
import PriceData

drawFrameFunction
  :: State
  -> HashMap.HashMap AsOf PriceData
  -> Scale
  -> Scale
  -> Scale
  -> Drawable
  -> IO (IO ())
drawFrameFunction _ _ _ _ _ d = do
  let vertices =
        VS.fromList [-0.99, -0.99, -0.99, 0.99, 0.99, 0.99, 0.99, -0.99]
  loadBuffer (dBufferId d) vertices
  putText ("frame vertices are: " <> show vertices)
  return
    (glDrawArrays GL_LINE_LOOP 0 (div (fromIntegral (VS.length vertices)) 2))

-- Add a frame for the chart.
frameDrawable :: VertexArrayId -> BufferId -> Drawable
frameDrawable vaId bId =
  Drawable
  { dDraw = return ()
  , dLoadBufferAndBuildDrawFunction = drawFrameFunction
  , dPreviousValue = Nothing
  , dCurrentValue =
      (\s _ -> ValueCursorPosition (stateCursorX s) (stateCursorY s))
  , dVertexArrayId = vaId
  , dBufferId = bId
  , dColour = green
  , dTransparency = Nothing
  , dType = Frame
  }
