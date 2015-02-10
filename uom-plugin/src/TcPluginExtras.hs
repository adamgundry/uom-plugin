{-# LANGUAGE RecordWildCards #-}

module TcPluginExtras
  ( -- * Re-exports from GHC typechecker plugin API
    TcPlugin(..)
  , TcPluginSolver
  , TcPluginM
  , tcPluginTrace
  , unsafeTcPluginTcM
  , tcLookupClass
  , tcLookupTyCon
  , newFlexiTyVar
  , isTouchableTcPluginM
  , zonkCt
  , matchFam

    -- * Wrappers
  , newUnique
  , newSimpleWanted

    -- * Extensions
  , tracePlugin
  , lookupModule
  , lookupName
  ) where

import Outputable
import TcRnTypes ( TcPlugin(..), TcPluginSolver, TcPluginResult(..) )
import TcRnMonad ( Ct, CtOrigin )
import TcPluginM

import Type
import Unique

import Module
import Name
import FastString

import qualified TcRnMonad
import qualified TcMType


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
    found_module <- findImportedModule mod_nm $ Just pkg
    case found_module of
      Found _ md -> return md
      _          -> do found_module' <- findImportedModule mod_nm $ Just $ fsLit "this"
                       case found_module' of
                         Found _ md -> return md
                         _          -> error $ "Unable to resolve module looked up by plugin: " ++ moduleNameString mod_nm

lookupName :: Module -> OccName -> TcPluginM Name
lookupName md occ = lookupOrig md occ


newUnique :: TcPluginM Unique
newUnique = unsafeTcPluginTcM TcRnMonad.newUnique

newSimpleWanted :: CtOrigin -> PredType -> TcPluginM Ct
newSimpleWanted orig = unsafeTcPluginTcM . TcMType.newSimpleWanted orig
