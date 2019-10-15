{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Sem.Metadata where

import Data.Function
import Polysemy
import Polysemy.State
import Types
import Prelude hiding (lookup)
import Data.Map (Map)
import qualified Data.Map as M

data Metadata m a where
  Link   :: Name -> Hash -> Metadata m ()
  Unlink :: Name -> Metadata m ()
  Lookup :: Name -> Metadata m (Maybe Hash)
  Uplook :: Hash -> Metadata m [Name]

makeSem ''Metadata


lookupAndKnow :: Member Metadata r => Name -> Sem r (Maybe (Name, Hash))
lookupAndKnow n = fmap (n, ) <$> lookup n

expandMetadata
    :: State (Map Name Hash) `Member` r
    => Sem (Metadata ': r) a
    -> Sem r a
expandMetadata = interpret $ \case
  Link name hash -> modify $ M.insert name hash
  Unlink name -> modify $ M.delete name
  Lookup name -> fmap (M.lookup name) get
  Uplook hash -> do
    assocs <- gets M.assocs
    pure . fmap fst $ filter ((== hash) . snd) assocs

