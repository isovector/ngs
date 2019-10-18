{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

module Sem.Metadata where

import Data.Maybe
import Data.Bool
import System.Directory
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Polysemy
import qualified Prelude as P
import           Prelude hiding (writeFile, readFile)
import           Types

data Metadata m a where
  Link   :: Name -> Hash -> Metadata m ()
  Unlink :: Name -> Metadata m ()
  Lookup :: Name -> Metadata m (Maybe Hash)
  Uplook :: Hash -> Metadata m [Name]

makeSem ''Metadata


data Filesystem m a where
  WriteFile :: FilePath -> ByteString -> Filesystem m ()
  ReadFile  :: FilePath -> Filesystem m (Maybe ByteString)
  DeleteFile :: FilePath -> Filesystem m ()
  Grep :: FilePath -> ByteString -> Filesystem m [FilePath]

makeSem ''Filesystem


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


filesystemToIO
    :: Member (Embed IO) r
    => Sem (Filesystem ': r) a
    -> Sem r a
filesystemToIO = interpret $ \case
  WriteFile file contents -> embed $ P.writeFile file $ BS.unpack contents
  ReadFile file -> embed $
    doesFileExist file >>= \case
      False -> pure Nothing
      True -> Just . BS.pack <$> P.readFile file
  DeleteFile file -> embed $ removeFile file
  Grep path contents -> embed $
    fmap catMaybes . traverse
      (\p ->
        fmap (bool Nothing (Just p) . (== BS.unpack contents))
          $ P.readFile p
      ) =<< listDirectory path


makeNamePath :: Name -> FilePath
makeNamePath (Name name) = magicPrefix <> name

fileToName :: FilePath -> Name
fileToName (drop (length magicPrefix) -> name) = Name name

magicPrefix :: String
magicPrefix = "dest/names/"

