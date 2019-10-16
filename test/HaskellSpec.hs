module HaskellSpec where

import Polysemy
import Polysemy.State
import Polysemy.Error
import Test.Hspec
import Lang
import Sem.Metadata
import Languages.Haskell
import Data.Map (Map)
import qualified Data.Map as M
import Types

spec :: Spec
spec = describe "Haskell language adapter" $ do
  it "should not crash" $ do
    let myMetadata :: Map Name Hash
        myMetadata = M.fromList
          [ (Name "turtles", Hash "123456")
          ]
    z <- runM . evalState myMetadata
         . runError
         . expandMetadata
         $ do
      n <- normalise @Haskell
         $ SourceCode
         $ unlines
             [ "foo :: ()"
             , "foo = turtles 5 6"
             ]
      traverse render n
    z `shouldBe` Right []



