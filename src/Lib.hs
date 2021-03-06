module Lib where

import qualified Data.ByteString.Char8 as BS
import           Polysemy
import           Polysemy.Input
import           Polysemy.Trace
import           Prelude hiding (lookup)
import           Sem.Filesystem
import           Sem.HashStore
import           Sem.Metadata
import           Sem.Stage (runStage, stage, commit, reset)
import qualified Sem.Stage
import           Types


data Command
  = NameToHash String -- --> Maybe String
  | GetHash String
  | Stage String String
  | Commit
  | Error String
  deriving (Eq, Ord, Show, Read)


main :: IO ()
main = runM
     . runInputSem (embed $ readLn @Command)
     . traceToIO
     . filesystemToIO
     . metadataToFilesystem
     . hashStoreToFilesystem
     . runStage
     $ do
  reset
  eventLoop


eventLoop
    :: Members '[ Sem.Stage.Stage
                , HashStore
                , Metadata
                , Input Command
                , Trace
                ] r
    => Sem r ()
eventLoop = do
  cmd <- input
  case cmd of
    NameToHash name -> do
      z <- fmap (fmap $ BS.unpack . getHash) $ lookup $ Name name
      trace $ show z
      eventLoop
    GetHash hash -> do
      z <- retrieve $ Hash $ BS.pack hash
      trace $ show $ fmap (BS.unpack . getAST) z
      eventLoop
    Stage name ast -> do
      stage (Name name) (AST $ BS.pack ast)
      eventLoop
    Commit -> do
      commit
      eventLoop
    Error _ -> pure ()

