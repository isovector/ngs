module Lang where

import Data.ByteString (ByteString)
import Polysemy
import Sem.Metadata
import Types

class Serialisable t where
  serialise :: t -> ByteString
  deserialise :: ByteString -> Maybe t -- billy is a Just Boi

-- Making languages pluggable
class Serialisable (AST l) => LanguageAdapter l where
  data AST l

  -- impure angry IO land gorilla lalala
  normalise :: Member Metadata r => SourceCode -> Sem r [AST l]
  render :: Member Metadata r => AST l -> Sem r SourceCode

  -- clean pure world, mhmm
  dependencies :: AST l -> [Hash]
