{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

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
import GLFWHelpers
import OpenGLHelpers
import Scale
import Types
import Drawable
import Screen
import Frame

main :: IO ()
main = putStrLn "TODO"

debugUsingSingleBuffer
    :: ((VertexArrayId, BufferId, VertexArrayId, BufferId, VertexArrayId, BufferId) -> IO a)
    -> IO a
-- debugUsingSingleBuffer f = withVertexArray (\vaId -> do f vaId)
debugUsingSingleBuffer f =
    withVertexArray $
    \fvaid fbid ->
         withVertexArray $
         \svaid sbid ->
              withVertexArray $
              \_ _ ->
                   withVertexArray $
                   \_ _ ->
                        withVertexArray $
                        \_ _ ->
                             withVertexArray $
                             \vaId bId ->
                                  f (fvaid, fbid, svaid, sbid, vaId, bId)

debugRenderer
    :: Window
    -> ColorUniformLocation
    -> State
    -> (VertexArrayId, BufferId, VertexArrayId, BufferId, VertexArrayId, BufferId)
    -> IO (VertexArrayId, BufferId, VertexArrayId, BufferId, VertexArrayId, BufferId)
debugRenderer win colorUniformLocation state (fvaid,fbid,svaid,sbid,lvaid,lbid) = do
    let vertices =
            (VS.fromList
                 [ -1
                 , (0 :: GLfloat)
                 , -0.5
                 , 1
                 , 0
                 , 0
                 , 0
                 , 0
                 , 0.5
                 , 1
                 , 1
                 , 0
                 , 0.5
                 , 0
                 , 1
                 , -1
                 , 0
                 , -1])
    putStrLn ("Vertex Array Id: " ++ show svaid)
    justDrawThis
        win
        colorUniformLocation
        vertices
        (div (VS.length vertices) 2)
        GL_TRIANGLES
        1
        0
        0
        1
        svaid
        sbid
    threadDelay (2 * 1000 * 1000)
    let d = screenDrawable svaid sbid
    --     (dDraw s) -- this works
    drawUsingVertexArray
        win
        colorUniformLocation
        (dVertexArrayId d)
        (dColour d)
        (dTransparency d)
        (dDraw d)
    let f = frameDrawable fvaid fbid
    loadBuffer (dBufferId f) vertices
    drawUsingVertexArray
        win
        colorUniformLocation
        (dVertexArrayId f)
        (dColour f)
        (dTransparency f)
        (glDrawArrays GL_LINES 0 8)
    --     let f = frameDrawable vaId
    --     loadUsingVertexArray (dVertexArrayId f) vertices
    --     drawUsingVertexArray
    --         win
    --         colorUniformLocation
    --         (dVertexArrayId f)
    --         (dColour f)
    --         (dTransparency f)
    --         (glDrawArrays GL_LINES 0 8)
    -- below works too
    --     glClearColor 0.05 0.05 0.05 1
    --     glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
    --     glDrawArrays GL_LINES 0 8
    GLFW.swapBuffers
        win
    glFlush  -- not necessary, but someone recommended it
    threadDelay (2 * 1000 * 1000)
    return (fvaid, fbid, svaid, sbid, lvaid, lbid)
