module Sem.Ghc
  ( runGHC
  , getBindingGroups
  , Ghc
  ) where

import Polysemy
import Polysemy.IO
import GhcMonad
import Types
import Language.Haskell.GHC.ExactPrint.Parsers
import GHC
import NameSet
import TcRnDriver
import RnSplice
import TcRnTypes
import TcRnMonad
import Outputable hiding ((<>))
import Bag
import RnSource
import HscTypes
import FastString
import SrcLoc
import Data.Maybe
import Module
import ErrUtils


getBindingGroups :: Members '[Embed Ghc, Embed IO] r => SourceCode -> Sem r (HsGroup GhcRn)
getBindingGroups (SourceCode src) = do
  z <- embed $ parseModuleFromString "magic" src
  case z of
    Right (_, L _ psrc) -> do
      embed $ tcm2 @Ghc $ tc_rn_src_decls $ hsmodDecls psrc





runGHC :: FilePath -> Sem '[Embed IO, Embed Ghc] a -> IO a
runGHC file m = do
  runGhc (Just "/home/sandy/.stack/programs/x86_64-linux/ghc-tinfo6-8.6.5/lib/ghc-8.6.5") $ runM $ embedToMonadIO m



tc_rn_src_decls :: [LHsDecl GhcPs]
                -> TcM (HsGroup GhcRn)
-- Loops around dealing with each top level inter-splice group
-- in turn, until it's dealt with the entire module
tc_rn_src_decls ds
 = {-# SCC "tc_rn_src_decls" #-}
   do { (first_group, group_tail) <- findSplice ds
                -- If ds is [] we get ([], Nothing)

        -- Deal with decls up to, but not including, the first splice
      ; (tcg_env, rn_decls) <- rnTopSrcDecls first_group
                -- rnTopSrcDecls fails if there are any errors

        -- Get TH-generated top-level declarations and make sure they don't
        -- contain any splices since we don't handle that at the moment
        --
        -- The plumbing here is a bit odd: see Trac #10853
      ; th_topdecls_var <- fmap tcg_th_topdecls getGblEnv
      ; th_ds <- readTcRef th_topdecls_var
      ; writeTcRef th_topdecls_var []

      ; (tcg_env, rn_decls) <-
            if null th_ds
            then return (tcg_env, rn_decls)
            else do { (th_group, th_group_tail) <- findSplice th_ds
                    ; case th_group_tail of
                        { Nothing -> return () ;
                        ; Just (SpliceDecl _ (L loc _) _, _)
                            -> setSrcSpan loc $
                               addErr (text "Declaration splices are not permitted inside top-level declarations added with addTopDecls")
                        ; Just (XSpliceDecl _, _) -> panic "tc_rn_src_decls"
                        } ;

                    -- Rename TH-generated top-level declarations
                    ; (tcg_env, th_rn_decls) <- setGblEnv tcg_env $
                      rnTopSrcDecls th_group

                    -- Dump generated top-level declarations
                    ; let msg = "top-level declarations added with addTopDecls"
                    ; traceSplice $ SpliceInfo { spliceDescription = msg
                                               , spliceIsDecl    = True
                                               , spliceSource    = Nothing
                                               , spliceGenerated = ppr th_rn_decls }

                    ; return (tcg_env, appendGroups rn_decls th_rn_decls)
                    }
      ; pure rn_decls
      }


tcm2 :: GhcMonad m => TcM a -> m a
tcm2 z = do
  hsc_env <- getSession
  (x, m) <- liftIO $
    initTc
      hsc_env
      HsSrcFile True
      (mkModule (DefiniteUnitId $ DefUnitId $ InstalledUnitId $ fsLit "hi") $ mkModuleName "hi")
      (mkRealSrcSpan (mkRealSrcLoc (fsLit "ok") 0 0)
                     (mkRealSrcLoc (fsLit "ok") 0 0))
      z
  -- pprPanic "x" $ vcat $ fmap pprLocErrMsg $ (bagToList (fst x) <> bagToList (snd x))
  pure $ fromJust m

