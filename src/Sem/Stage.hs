{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE TemplateHaskell #-}

module Sem.Stage where

import Data.Foldable
import Polysemy
import Polysemy.State
import Sem.HashStore
import Sem.Metadata
import Types


data Stage m a where
  Stage :: Name -> AST -> Stage m ()
  Reset :: Stage m ()
  Commit :: Stage m ()

makeSem ''Stage


runStage
    :: Members '[HashStore, Metadata] r
    => Sem (Stage ': r) a
    -> Sem r a
runStage = evalState @[(Name, AST)] [] . reinterpret \case
  Stage name ast -> modify ((name, ast) :)
  Reset -> put []
  Commit -> do
    asts <- get
    for_ asts $ \(name, ast) -> do
      hash <- store ast
      link name hash
    put []

