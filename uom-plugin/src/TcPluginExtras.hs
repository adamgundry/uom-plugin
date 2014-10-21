{-# LANGUAGE RecordWildCards #-}

module TcPluginExtras
  ( -- * Re-exports from GHC typechecker plugin API
    TcPlugin(..)
  , TcPluginSolver
  , TcPluginM
  , tcPluginTrace
  , unsafeTcPluginTcM

    -- * Extensions
  , simplePlugin
  , tracePlugin
  , lookupRdrName

    -- * Wrappers
  , tcLookupTyCon
  , newGlobalBinder
  , newFlexiTyVarTy
  , isTouchableTcPluginM
  , matchFam
  ) where

import DynamicLoading ( lookupRdrNameInModuleForPlugins )
import HscTypes  ( HscEnv )
import qualified IfaceEnv  ( newGlobalBinder )
import Kind      ( Kind )
import Module    ( Module, ModuleName, moduleNameString )
import Name      ( Name )
import OccName   ( OccName, occNameString )
import Outputable
import RdrName   ( RdrName, rdrNameOcc )
import SrcLoc    ( SrcSpan )
import qualified TcEnv     ( tcLookupTyCon )
import qualified TcMType ( newFlexiTyVarTy )
import TcRnMonad ( tcPluginIO, tcPluginTrace, isTouchableTcM, getTopEnv )
import TcRnTypes ( TcPlugin(..), TcPluginSolver, TcPluginResult(..), TcPluginM, unsafeTcPluginTcM )
import TcType    ( TcType, TcTyVar )

import TyCon
import FamInst
import FamInstEnv
import CoAxiom
import TcEvidence
import Pair
import VarSet
import TypeRep




simplePlugin :: TcPluginSolver -> TcPlugin
simplePlugin solver = TcPlugin { tcPluginInit  = const $ return ()
                               , tcPluginSolve = const solver
                               , tcPluginStop  = const $ return ()
                               }

tracePlugin :: String -> TcPlugin -> TcPlugin
tracePlugin s TcPlugin{..} = TcPlugin { tcPluginInit  = traceInit
                                      , tcPluginSolve = traceSolve
                                      , tcPluginStop  = traceStop
                                      }
  where
    traceInit  xs      = tcPluginTrace ("tcPluginInit " ++ s) (ppr xs) >> tcPluginInit xs
    traceStop  z       = tcPluginTrace ("tcPluginStop " ++ s) empty    >> tcPluginStop z

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



lookupRdrName :: ModuleName -> RdrName -> TcPluginM Name
lookupRdrName mod rdr = do
  hsc_env <- unsafeTcPluginTcM getTopEnv
  mb_name <- tcPluginIO $ lookupRdrNameInModuleForPlugins hsc_env mod rdr
  case mb_name of
    Just name -> return name
    Nothing   -> error $ "TcPluginExtras.lookupRdrName: missing "
                          ++ moduleNameString mod ++ "." ++ occNameString (rdrNameOcc rdr)


tcLookupTyCon :: Name -> TcPluginM TyCon
tcLookupTyCon = unsafeTcPluginTcM . TcEnv.tcLookupTyCon


newGlobalBinder :: Module -> OccName -> SrcSpan -> TcPluginM Name
newGlobalBinder m o s = unsafeTcPluginTcM $ IfaceEnv.newGlobalBinder m o s


newFlexiTyVarTy :: Kind -> TcPluginM TcType
newFlexiTyVarTy = unsafeTcPluginTcM . TcMType.newFlexiTyVarTy

isTouchableTcPluginM :: TcTyVar -> TcPluginM Bool
isTouchableTcPluginM = unsafeTcPluginTcM . isTouchableTcM


-- This is just TcSMonad.matchFam, but written to work in TcM instead
matchFam :: TyCon -> [Type] -> TcPluginM (Maybe (TcCoercion, TcType))
matchFam tycon args
  | isOpenSynFamilyTyCon tycon
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
