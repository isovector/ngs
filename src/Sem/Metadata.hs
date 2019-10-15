{-# LANGUAGE TemplateHaskell #-}

module Sem.Metadata where

import Polysemy
import Types

data Metadata m a where
  Link   :: Name -> Hash -> Metadata m ()
  Unlink :: Name -> Metadata m ()
  Lookup :: Name -> Metadata m (Maybe Hash)
  Uplook :: Hash -> Metadata m [Name]

makeSem ''Metadata

