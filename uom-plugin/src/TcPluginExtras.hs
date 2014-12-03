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
import TcRnTypes ( TcPlugin(..), TcPluginSolver, TcPluginResult(..) ) -- TODO: move imports
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
import IfaceEnv
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



-- This is just TcSMonad.matchFam, but written to work in TcM instead
matchFam :: TyCon -> [Type] -> TcPluginM (Maybe (TcCoercion, TcType))
matchFam tycon args
  | isOpenTypeFamilyTyCon tycon
  = do { fam_envs <- unsafeTcPluginTcM tcGetFamInstEnvs
       ; let mb_match = tcLookupFamInst fam_envs tycon args
       ; tcPluginTrace "lookupFamInst" $
                  vcat [ ppr tycon <+> ppr args
                       , pprTvBndrs (varSetElems (tyVarsOfTypes args))
                       , ppr mb_match ]
       ; case mb_match of
           Nothing -> return Nothing
           Just (FamInstMatch { fim_instance = famInst
                              , fim_tys      = inst_tys })
             -> let co = mkTcUnbranchedAxInstCo Nominal (famInstAxiom famInst) inst_tys
                    ty = pSnd $ tcCoercionKind co
                in return $ Just (co, ty) }

  | Just ax <- isClosedSynFamilyTyCon_maybe tycon
  , Just (ind, inst_tys) <- chooseBranch ax args
  = let co = mkTcAxInstCo Nominal ax ind inst_tys
        ty = pSnd (tcCoercionKind co)
    in return $ Just (co, ty)

  | Just ops <- isBuiltInSynFamTyCon_maybe tycon =
    return $ do (r,ts,ty) <- sfMatchFam ops args
                return (mkTcAxiomRuleCo r ts [], ty)

  | otherwise
  = return Nothing


lookupModule :: ModuleName -> FastString -> TcPluginM Module
lookupModule mod_nm pkg = do
    hsc_env <- getTopEnv
    found_module <- tcPluginIO $ findImportedModule hsc_env mod_nm $ Just pkg
    case found_module of
      Found _ md -> return md
      _          -> error $ "Unable to resolve module looked up by plugin: " ++ moduleNameString mod_nm

lookupName :: Module -> OccName -> TcPluginM Name
lookupName md occ = unsafeTcPluginTcM $ newGlobalBinder md occ loc
  where
    loc = mkGeneralSrcSpan (fsLit "<typechecker plugin>")
