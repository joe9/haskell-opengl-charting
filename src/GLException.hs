{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module GLException
  ( GLException(..)
  ) where

import Control.Exception.Safe
import Protolude
import Quine.GL.Error

data GLException
  = ShaderProgramCompilationFailed Text
                                   Text
                                   Text
                                   [Error]
  | ProgramCompilationFailed Text
                             [Error]
  | GLFWInitFailed
  deriving (Eq, Show, Typeable)

instance Exception GLException
