module Lib where

import qualified Data.ByteString.Char8 as BS
import           Polysemy
import           Prelude hiding (lookup)
import           Sem.Filesystem
import           Sem.HashStore
import           Sem.Metadata
import           Sem.Stage
import           Types


data Command
  = NameToHash String -- --> Maybe String
  | Error String
  deriving (Eq, Ord, Show, Read)


main :: IO ()
main = runM . filesystemToIO . metadataToFilesystem . hashStoreToFilesystem . runStage $ do
  cmd <- embed $ readLn @Command
  case cmd of
    NameToHash name -> do
      z <- fmap (fmap $ BS.unpack . getHash) $ lookup $ Name name
      embed $ print z

