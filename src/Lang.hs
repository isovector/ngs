module Lang where

import Polysemy
import Sem.Metadata
import Types


-- Making languages pluggable
class LanguageAdapter l where
  data AST l

  -- impure angry IO land lalala
  normalise    :: Member Metadata r => SourceCode -> Sem r [AST l]
  serialise    :: Member Metadata r => AST l -> Sem r SourceCode

  -- clean pure world, mhmm
  hash         :: AST l -> Hash
  dependencies :: AST l -> [Hash]

