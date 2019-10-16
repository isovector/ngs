{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns      #-}

module Languages.Haskell where

import           Bag
import           Control.Arrow (second, (***))
import qualified Data.ByteString.Char8 as BS8
import           Data.Generics
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           GHC (TypecheckedModule (..), unLoc, RdrName (..), noLoc)
import           HsSyn
import           Lang
import           OccName
import           Outputable (ppr, showSDocUnsafe)
import           Polysemy
import           Polysemy.Error
import           Prelude hiding (lookup)
import           Sem.Ghc
import           Sem.Metadata
import           Types


data Haskell

instance Serialisable (AST Haskell) where
  serialise (HaskellAST ast) = BS8.pack $ showSDocUnsafe $ ppr ast
  deserialise = undefined

instance LanguageAdapter Haskell where
  data AST Haskell = HaskellAST (HsBindLR GhcRn GhcRn)
  type ExtraEffs Haskell = '[Embed IO]

  normalise src = do
    x <- embed
       . runGHC "."
       $ getBindingGroups src
    let binds = getBinds $ hs_valds x
    maybe_free_hashes <- fmap sequenceA $ traverse lookupAndKnow $ getFreeVars binds
    case maybe_free_hashes of
      Nothing -> throw MissingHashes
      Just free_hashes -> do
    -- TODO(sandy): there's a bug here where free vars are free in the function but not in the program itself. we actually want to replace all the free vars!!!
        pure $ fmap HaskellAST
             $ replaceFreeVars  -- here!
                 (M.fromList $ fmap (mkVarOcc . unName *** hashToOccName) free_hashes)
             $ binds

  -- TODO(sandy): make this thing dump names back into scope
  render (HaskellAST expr) =
    pure $ SourceCode $ showSDocUnsafe $ ppr expr


getFreeVars :: Data a => a -> [Name]
getFreeVars =
  everything (<>) $
    mkQ [] $ \case
      HsUnboundVar _ (OutOfScope occ _) -> [Name $ occNameString occ]
      (_ :: HsExpr GhcRn)               -> []


replaceFreeVars :: Data a => Map OccName OccName -> a -> a
replaceFreeVars replacements =
  everywhere $ mkT $ \case
    HsUnboundVar ext (OutOfScope occ gbl) ->
      HsUnboundVar ext (OutOfScope (replacements M.! occ) gbl)
    (a :: HsExpr GhcRn) -> a


-- | NOTE: this is an optic (it's a prism) thanks william for pointing this deep fact out. you are the king. king william.
hashToOccName :: Hash -> OccName
hashToOccName (Hash hash) = mkVarOcc $ hashPrefix <> hash

occNameToHash :: OccName -> Maybe Hash
occNameToHash (occNameString -> occ)
  | isPrefixOf hashPrefix occ  =
      Just $ Hash $ drop (length hashPrefix) occ
  | otherwise = Nothing

hashPrefix :: String
hashPrefix = "$$$42069_"



getBinds :: HsValBinds GhcRn -> [HsBindLR GhcRn GhcRn]
getBinds (ValBinds _ bs _) =
  fmap unLoc $ bagToList bs
getBinds (XValBindsLR (NValBinds bs _)) =
  fmap unLoc $ bagToList . snd =<< bs

