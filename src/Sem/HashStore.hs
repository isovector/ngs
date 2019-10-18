{-# LANGUAGE TemplateHaskell #-}

module Sem.HashStore where

import qualified Data.ByteString.Char8 as BS
import           Data.Hashable
import           Polysemy
import           Prelude hiding (writeFile, readFile)
import           Sem.Filesystem
import           Types


data HashStore m a where
  Store    :: AST -> HashStore m Hash
  Retrieve :: Hash -> HashStore m (Maybe AST)

makeSem ''HashStore


hashStoreToFilesystem
    :: Member Filesystem r
    => Sem (HashStore ': r) a
    -> Sem r a
hashStoreToFilesystem = interpret $ \case
  Store ast@(AST contents) -> do
    let h = Hash $ BS.pack $ "hash_" <> show (hash ast)
    writeFile (makeHashPath h) contents
    pure h
  Retrieve h -> fmap (fmap AST) $ readFile $ makeHashPath h



makeHashPath :: Hash -> FilePath
makeHashPath (Hash name) = magicPrefix <> BS.unpack name

magicPrefix :: String
magicPrefix = "dest/hash/"


