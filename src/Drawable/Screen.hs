{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Drawable.Screen where

--   https://ghc.haskell.org/trac/ghc/wiki/Commentary/Packages/PackageImportsProposal
import Data.Bits
import Data.Colour.Names
import "gl" Graphics.GL
import Prelude           hiding (init)
import Protolude

--
import Drawable
import OpenGLHelpers

screenDrawable :: VertexArrayId -> BufferId -> Drawable
screenDrawable vaId bId =
  let drawFunction = do
        glClearColor 0.05 0.05 0.05 1
        glClear
          (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT)
  in Drawable
     { dDraw = drawFunction
     , dPreviousValue = Just ValueEmpty
     , dCurrentValue = \_ _ -> ValueEmpty
     , dLoadBufferAndBuildDrawFunction = (\_ _ _ _ _ _ -> return drawFunction)
     , dVertexArrayId = vaId
     , dBufferId = bId
     , dColour = red
     , dTransparency = Nothing
     , dType = Screen
     }
