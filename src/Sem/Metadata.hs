{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

module Sem.Metadata where

import Data.Maybe
import Polysemy
import Prelude hiding (writeFile, readFile)
import Sem.Filesystem
import Types


data Metadata m a where
  Link   :: Name -> Hash -> Metadata m ()
  Unlink :: Name -> Metadata m ()
  Lookup :: Name -> Metadata m (Maybe Hash)
  Uplook :: Hash -> Metadata m [Name]

makeSem ''Metadata


metadataToFilesystem
    :: Member Filesystem r
    => Sem (Metadata ': r) a
    -> Sem r a
metadataToFilesystem = interpret $ \case
  Link name (Hash hash) ->
    writeFile (makeNamePath name) hash
  Unlink name ->
    deleteFile $ makeNamePath name
  Lookup name ->
    fmap (fmap Hash) . readFile $ makeNamePath name
  Uplook (Hash hash) ->
    fmap (fmap fileToName) $ grep magicPrefix hash



makeNamePath :: Name -> FilePath
makeNamePath (Name name) = magicPrefix <> name

fileToName :: FilePath -> Name
fileToName (drop (length magicPrefix) -> name) = Name name

magicPrefix :: String
magicPrefix = "dest/names/"

