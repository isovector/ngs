module Lib where

import Polysemy
import Sem.Metadata
import Prelude hiding (lookup)
import Types
import qualified Data.ByteString.Char8 as BS


data Command
  = NameToHash String -- --> Maybe String
  | Error String
  deriving (Eq, Ord, Show, Read)


main :: IO ()
main = runM . filesystemToIO . metadataToFilesystem $ do
  cmd <- embed $ readLn @Command
  case cmd of
    NameToHash name -> do
      z <- fmap (fmap (BS.unpack . getHash)) $ lookup $ Name name
      embed $ print z

