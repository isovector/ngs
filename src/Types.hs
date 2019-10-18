module Types where

import Data.ByteString (ByteString)

newtype Name = Name String
newtype Hash = Hash ByteString
newtype SourceCode = SourceCode String
