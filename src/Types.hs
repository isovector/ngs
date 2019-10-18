module Types where

import Data.ByteString (ByteString)

newtype Name = Name String
newtype Hash = Hash { getHash :: ByteString }
newtype SourceCode = SourceCode String
