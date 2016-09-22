{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Screen where

--   https://ghc.haskell.org/trac/ghc/wiki/Commentary/Packages/PackageImportsProposal
import Control.Concurrent
import Control.Concurrent.Async
import Data.Bits
import Data.Colour.Names
import Data.IORef
import Data.Time.Clock.POSIX
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import "gl" Graphics.GL
import Graphics.UI.GLFW as GLFW
import Prelude hiding (init)
import System.Random
--
import Drawable
import PriceGraphOpenGL
import VolumeGraphOpenGL
import GLFWStuff
import MyDataUnboxedVector
import OpenGLStuff
import ScaleDataUnboxedVector
import TypesOpenGL

screenDrawable :: VertexArrayId -> BufferId -> Drawable
screenDrawable vaId bId =
    let drawFunction = do
            glClearColor 0.05 0.05 0.05 1
            glClear
                (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT .|.
                 GL_STENCIL_BUFFER_BIT)
    in Drawable
       { dDraw = drawFunction
       , dPreviousValue = Just ValueEmpty
       , dCurrentValue = \_ _ ->
                              ValueEmpty
       , dLoadBufferAndBuildDrawFunction = (\_ _ _ _ _ _ ->
                                                 return drawFunction)
       , dVertexArrayId = vaId
       , dBufferId = bId
       , dColour = red
       , dTransparency = Nothing
       , dType = Screen
       }
