{-# LANGUAGE TemplateHaskell #-}

module Sem.Filesystem where

import           Data.Bool
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import           Polysemy
import           Polysemy.State
import qualified Prelude as P
import           Prelude hiding (writeFile, readFile)
import           System.Directory


data Filesystem m a where
  WriteFile  :: FilePath -> ByteString -> Filesystem m ()
  ReadFile   :: FilePath -> Filesystem m (Maybe ByteString)
  DeleteFile :: FilePath -> Filesystem m ()
  Grep       :: FilePath -> ByteString -> Filesystem m [FilePath]

makeSem ''Filesystem


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


pureFilesystem
    :: Member (State (M.Map FilePath ByteString)) r
    => Sem (Filesystem ': r) a
    -> Sem r a
pureFilesystem = interpret $ \case
  WriteFile file contents -> modify $ M.insert file contents
  ReadFile file -> gets $ M.lookup file
  DeleteFile file -> modify $ M.delete file
  Grep path thing -> do
    assocs <- gets M.assocs
    pure $ fmap fst $ filter (\(fp, c) -> fp `isPrefixOf` path && thing == c) assocs

