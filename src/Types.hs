{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Data.Hashable
import Data.ByteString (ByteString)

newtype Name = Name String
newtype Hash = Hash { getHash :: ByteString }
newtype SourceCode = SourceCode String

newtype AST = AST { getAST :: ByteString }
  deriving Hashable

