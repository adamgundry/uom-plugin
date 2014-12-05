{-# LANGUAGE RecordWildCards #-}

module TcPluginExtras
  ( -- * Re-exports from GHC typechecker plugin API
    TcPlugin(..)
  , TcPluginSolver
  , TcPluginM
  , tcPluginTrace
  , unsafeTcPluginTcM

    -- * Extensions
  , tracePlugin
  , lookupModule
  , lookupName

    -- * Wrappers
  , tcLookupTyCon
  , newFlexiTyVar
  , isTouchableTcPluginM
  , zonkCt
  , matchFam
  ) where

import Outputable
import TcRnTypes ( TcPlugin(..), TcPluginSolver, TcPluginResult(..) )
import TcType    ( TcType )
import TcPluginM

import TyCon
import FamInst
import FamInstEnv
import CoAxiom
import TcEvidence
import Pair
import VarSet
import TypeRep

import Module
import Name
import Finder
import SrcLoc
import FastString



tracePlugin :: String -> TcPlugin -> TcPlugin
tracePlugin s TcPlugin{..} = TcPlugin { tcPluginInit  = traceInit
                                      , tcPluginSolve = traceSolve
                                      , tcPluginStop  = traceStop
                                      }
  where
    traceInit    = tcPluginTrace ("tcPluginInit " ++ s) empty >> tcPluginInit
    traceStop  z = tcPluginTrace ("tcPluginStop " ++ s) empty >> tcPluginStop z

    traceSolve z given derived wanted = do
        tcPluginTrace ("tcPluginSolve start " ++ s)
                          (text "given   =" <+> ppr given
                        $$ text "derived =" <+> ppr derived
                        $$ text "wanted  =" <+> ppr wanted)
        r <- tcPluginSolve z given derived wanted
        case r of
          TcPluginOk solved new     -> tcPluginTrace ("tcPluginSolve ok " ++ s)
                                           (text "solved =" <+> ppr solved
                                         $$ text "new    =" <+> ppr new)
          TcPluginContradiction bad -> tcPluginTrace ("tcPluginSolve contradiction " ++ s)
                                           (text "bad =" <+> ppr bad)
        return r


lookupModule :: ModuleName -> FastString -> TcPluginM Module
lookupModule mod_nm pkg = do
    hsc_env <- getTopEnv
    found_module <- tcPluginIO $ findImportedModule hsc_env mod_nm $ Just pkg
    case found_module of
      Found _ md -> return md
      _          -> error $ "Unable to resolve module looked up by plugin: " ++ moduleNameString mod_nm

lookupName :: Module -> OccName -> TcPluginM Name
lookupName md occ = newGlobalBinder md occ loc
  where
    loc = mkGeneralSrcSpan (fsLit "<typechecker plugin>")
