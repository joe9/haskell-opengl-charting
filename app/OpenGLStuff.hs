{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE TypeFamilies              #-}

module OpenGLStuff where

-- | A port of the code presented at [Modern OpenGL with
-- Haskell](http://www.arcadianvisions.com/blog/?p=224) to use the
-- GLFW-b package.
import Prelude hiding (init)
-- import           System.Exit (exitFailure)
-- import Control.Applicative
import Control.Concurrent
import Control.Exception.Safe
import Control.Monad          (when)
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.IO
-- import Graphics.GLUtil
-- import Graphics.Rendering.OpenGL
-- import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
--   https://ghc.haskell.org/trac/ghc/wiki/Commentary/Packages/PackageImportsProposal
import           Data.Bits
import qualified Data.ByteString      as BS
import           Data.Colour.SRGB
import           Data.Colour.SRGB     ()
import           Data.Maybe
import qualified Data.Vector.Storable as V
import           "gl" Graphics.GL
import           Quine.Debug
import           Quine.GL.Error
--
import GLException

--   http://stackoverflow.com/questions/27407774/get-supported-glsl-versions
vertexShaderSource :: BS.ByteString
vertexShaderSource =
  BS.intercalate
    "\n"
    ["#version 330"
    ,"layout (location=0) in vec2 inPosition;"
    ,"void main(void) { "
    ,"  gl_Position = vec4(inPosition.x, inPosition.y, 0.0f, 1.0f);"
    ,"}"]

fragmentShaderSource :: BS.ByteString
fragmentShaderSource =
  BS.intercalate
    "\n"
    ["#version 330"
    ,"uniform vec4 color = vec4(1.0f,0.0f,0.0f,1.0f);"
    ,"out vec4 out_Color;"
    ,"void main(void) {out_Color = color;}"]

type ColorUniformLocation = GLint

data Picture =
  Picture (V.Vector GLfloat)
          GLenum
          (RGB GLfloat,GLfloat)

drawPictures
  :: Window -> ColorUniformLocation -> [Picture] -> IO ()
drawPictures window colorUniformLocation ps =
  do -- previousmt <- GLFW.getTime
     glClearColor 0.05 0.05 0.05 1
     glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT .|.
              GL_STENCIL_BUFFER_BIT)
     mapM_ (drawPicture window colorUniformLocation) ps
     GLFW.swapBuffers window
     glFlush  -- not necessary, but someone recommended it
     -- mt <- GLFW.getTime
--      putStrLn ("time taken to draw: " ++
--                show (1000 * (fromMaybe 0 mt - fromMaybe 0 previousmt)) ++
--                " milliseconds")

drawPicture
  :: Window -> ColorUniformLocation -> Picture -> IO ()
drawPicture _ colorUniformLocation (Picture vertices drawType (RGB r g b,t)) =
  do loadColor colorUniformLocation r g b t
     loadBuffer vertices
     glDrawArrays drawType
                  0
                  (fromIntegral (V.length vertices))

--   GLFW.pollEvents
colorUniformLocationInProgram
  :: ProgramId -> IO ColorUniformLocation
colorUniformLocationInProgram programId =
  withCString
    "color"
    (\namePtr -> checkGLErrors (glGetUniformLocation programId namePtr))

loadColor :: ColorUniformLocation
          -> GLfloat
          -> GLfloat
          -> GLfloat
          -> GLfloat
          -> IO ()
loadColor colorUniformLocation r g b t =
  glUniform4f colorUniformLocation r g b t

loadBuffer :: V.Vector GLfloat -> IO ()
loadBuffer bufferData =
  V.unsafeWith
    bufferData
    (\ptr ->
       glBufferData
         GL_ARRAY_BUFFER
         (fromIntegral (sizeOf (undefined :: GLfloat) * V.length bufferData))
         (castPtr ptr)
         GL_STREAM_DRAW)

withVertexArrayObject :: IO a -> IO a
withVertexArrayObject f =
  bracket (alloca (\vertexArrayObjectPtr ->
                     do checkGLErrors $
                          glGenVertexArrays 1 vertexArrayObjectPtr
                        peek vertexArrayObjectPtr))
          (\vertexArrayObjectId ->
             alloca (\vertexArrayObjectPtr ->
                       do poke vertexArrayObjectPtr vertexArrayObjectId
                          checkGLErrors $
                            glDeleteVertexArrays 1 vertexArrayObjectPtr))
          (\vertexArrayObjectId ->
             do glBindVertexArray vertexArrayObjectId
                withVertexBufferObject f)

type AttributeIndex = GLuint

withVertexAttribArray
  :: AttributeIndex -> IO d -> IO d
withVertexAttribArray attributeIndex f =
  bracket_ (checkGLErrors $ glEnableVertexAttribArray attributeIndex)
           (checkGLErrors $ glDisableVertexAttribArray attributeIndex)
           f

-- following https://www.opengl.org/wiki/Vertex_Specification
-- separating format specification from buffers ARB_vertex_attrib_binding
withVertexBufferObject :: IO d -> IO d
withVertexBufferObject f =
  withBufferObject $
  \bufferObjectId ->
    do
       -- hardcoding bindingindex to 0
       -- stride = 2, as all my vertexes have 2 coordinates (x & y)
       checkGLErrors $
         glBindVertexBuffer 0
                            bufferObjectId
                            0
                            (fromIntegral (2 * (sizeOf (undefined :: GLfloat))))
       checkGLErrors $ glVertexAttribFormat 0 2 GL_FLOAT GL_FALSE 0
       checkGLErrors $ glVertexAttribBinding 0 0
       withVertexAttribArray 0 f

--   https://www.opengl.org/wiki/Buffer_Object
-- bufferObjectType : mostly GL_ARRAY_BUFFER
-- usage : for axis and frame: GL_DYNAMIC_DRAW -- data changes occassionally
--                   others: GL_STREAM_DRAW -- data changes after every use
-- GLsizeiptr: It's not a pointer. It's an integral type the same size as a pointer.
--   http://stackoverflow.com/questions/26758129/glbufferdata-second-arg-is-glsizeiptr-not-glsizei-why
type BufferObjectId = GLuint

withBufferObject
  :: (BufferObjectId -> IO c) -> IO c
withBufferObject f =
  bracket (alloca (\bufferObjectPtr ->
                     do checkGLErrors $ glGenBuffers 1 bufferObjectPtr
                        peek bufferObjectPtr))
          (\bufferObjectId ->
             alloca (\bufferObjectPtr ->
                       do poke bufferObjectPtr bufferObjectId
                          checkGLErrors $ glDeleteBuffers 1 bufferObjectPtr))
          (\bufferObjectId ->
             do glBindBuffer GL_ARRAY_BUFFER bufferObjectId
                f bufferObjectId)

type ProgramId = GLuint

attachShader
  :: ProgramId -> ShaderType -> BS.ByteString -> IO () -> IO ()
attachShader programId shaderType shaderSource f =
  bracket (getShaderId shaderType shaderSource)
          (\shaderId ->
             glDetachShader programId shaderId >> glDeleteShader shaderId)
          (\shaderId -> checkGLErrors (glAttachShader programId shaderId) >> f)

-- withProgram :: (ProgramId -> IO r) -> IO r
withProgram :: (ProgramId -> IO a) -> IO a
withProgram f =
  installDebugHook >>
  bracket (checkGLErrors glCreateProgram)
          (\programId -> glUseProgram 0 >> glDeleteProgram programId)
          (\programId ->
             do attachShader
                  programId
                  Vertex
                  vertexShaderSource
                  (attachShader
                     programId
                     Fragment
                     fragmentShaderSource
                     (do glLinkProgram programId
                         -- check compile status
                         linkStatus <-
                           alloca (\linkStatusPtr ->
                                     do glGetProgramiv programId GL_LINK_STATUS linkStatusPtr
                                        peek linkStatusPtr)
                         infoLogLength <-
                           alloca (\infoLogLengthPtr ->
                                     do glGetProgramiv programId GL_INFO_LOG_LENGTH infoLogLengthPtr
                                        peek infoLogLengthPtr)
                         putStrLn ("infoLogLength: " ++ show infoLogLength)
                         infoLog <- getProgramInfoLog programId infoLogLength
                         putStrLn ("infoLog: " ++ infoLog)
                         if linkStatus == GL_TRUE
                            then checkGLErrors (glUseProgram programId)
                            else errors >>=
                                 (\es ->
                                    throw (ProgramCompilationFailed infoLog es))))
                f programId)

-- the infoLogLength includes the size of the null termination character
-- do not bother printing the infoLog if it just has the null
--   termination character
type InfoLogLength = GLsizei

getProgramInfoLog
  :: ProgramId -> InfoLogLength -> IO String
getProgramInfoLog programId infoLogLength
  | infoLogLength > 1 =
    alloca (\infoLogPtr ->
              do glGetProgramInfoLog programId infoLogLength nullPtr infoLogPtr
                 peekCString infoLogPtr)
  | otherwise = return "No Info Log"

getShaderInfoLog
  :: ShaderId -> InfoLogLength -> IO String
getShaderInfoLog shaderId infoLogLength
  | infoLogLength > 1 =
    alloca (\infoLogPtr ->
              do glGetShaderInfoLog shaderId infoLogLength nullPtr infoLogPtr
                 peekCString infoLogPtr)
  | otherwise = return "No Info Log"

-- sticking to using the raw gl calls as I can be sure where something
-- got messed up when things go wrong
--   http://book.realworldhaskell.org/read/interfacing-with-c-the-ffi.html
--   http://www.cse.unsw.edu.au/~chak/haskell/ffi/ffi/ffise5.html#x8-340005.9
-- https://www.opengl.org/wiki/Shader_Compile_Error
data ShaderType
  = Vertex
  | Fragment
  deriving (Eq,Show)

type ShaderId = GLuint

-- https://www.opengl.org/wiki/Shader_Compile_Error
getShaderId
  :: ShaderType -> BS.ByteString -> IO ShaderId
getShaderId shaderType shaderSource =
  let shaderTypeGL Vertex   = GL_VERTEX_SHADER
      shaderTypeGL Fragment = GL_FRAGMENT_SHADER
  in bracketOnError
       (checkGLErrors (glCreateShader (shaderTypeGL shaderType)))
       -- prevent leak
       glDeleteShader
       (\shaderId ->
          do BS.useAsCString
               shaderSource
               (\c_string ->
                  let gl_string = castPtr c_string :: Ptr GLchar
                  in withArray [gl_string]
                               (\ptrToArrayOfStrings ->
                                  glShaderSource shaderId 1 ptrToArrayOfStrings nullPtr))
             glCompileShader shaderId
             -- check compile status
             compileStatus <-
               alloca (\compileStatusPtr ->
                         do glGetShaderiv shaderId GL_COMPILE_STATUS compileStatusPtr
                            peek compileStatusPtr)
             putStrLn ("compileStatus: " ++ show compileStatus)
             threadDelay (1 * 1000 * 1000)
             infoLogLength <-
               alloca (\infoLogLengthPtr ->
                         do glGetShaderiv shaderId GL_INFO_LOG_LENGTH infoLogLengthPtr
                            peek infoLogLengthPtr)
             putStrLn ("infoLogLength: " ++ show infoLogLength)
             putStrLn ("infoLogLength: " ++ show infoLogLength)
             infoLog <- getShaderInfoLog shaderId infoLogLength
             putStrLn ("infoLog: " ++ infoLog)
             if compileStatus == GL_TRUE
                then return shaderId
                else errors >>=
                     (\es ->
                        throw (ShaderProgramCompilationFailed (show shaderType)
                                                              (show shaderSource)
                                                              infoLog
                                                              es)))

-- according to the docs, this should be a loop checking until there
-- are no errors
-- https://www.opengl.org/wiki/GLAPI/glGetError
checkGLErrors :: IO a -> IO a
checkGLErrors f = f >>= (\v -> throwErrors >> return v)

-- I do not care about the others
data BufferObjectType
  = ARRAY_BUFFER
  | ELEMENT_ARRAY_BUFFER

data BufferUsageType
  = STATIC_DRAW
  | DYNAMIC_DRAW
  | STREAM_DRAW

bufferObjectTypeToEnum :: (Eq a
                          ,Num a)
                       => BufferObjectType -> a
bufferObjectTypeToEnum ARRAY_BUFFER = GL_ARRAY_BUFFER
bufferObjectTypeToEnum ELEMENT_ARRAY_BUFFER = GL_ELEMENT_ARRAY_BUFFER

bufferUsageTypeToEnum :: (Eq a
                         ,Num a)
                      => BufferUsageType -> a
bufferUsageTypeToEnum STATIC_DRAW  = GL_STATIC_DRAW
bufferUsageTypeToEnum DYNAMIC_DRAW = GL_DYNAMIC_DRAW
bufferUsageTypeToEnum STREAM_DRAW  = GL_STREAM_DRAW
