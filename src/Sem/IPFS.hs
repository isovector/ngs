{-# LANGUAGE TemplateHaskell #-}

module Sem.IPFS where

import Polysemy
import Lang
import Types

data IPFS m a where
  GetIpfs :: LanguageAdapter l => Hash -> IPFS m (Maybe (AST l))
  PutIpfs :: LanguageAdapter l => AST l -> IPFS m ()

makeSem ''IPFS

