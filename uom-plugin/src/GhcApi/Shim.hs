{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}

module GhcApi.Shim
    (
      tyVarsOfType
    , tyVarsOfTypes
    , promoteTyCon

#if __GLASGOW_HASKELL__ >= 800
#if __GLASGOW_HASKELL__ < 802
    , pattern FunTy
#endif
    , mkEqPred
    , mkHEqPred
#endif

    , mkFunnyEqEvidence 
    ) where

import GhcApi

tyVarsOfType :: Type -> TyCoVarSet
tyVarsOfType = tyCoVarsOfType

tyVarsOfTypes :: [Type] -> TyCoVarSet
tyVarsOfTypes = tyCoVarsOfTypes

promoteTyCon :: TyCon -> TyCon
promoteTyCon = id

#if __GLASGOW_HASKELL__ >= 800
#if __GLASGOW_HASKELL__ < 802
pattern FunTy :: Type -> Type -> Type
pattern FunTy t v = ForAllTy (Anon t) v
#endif

mkEqPred :: Type -> Type -> Type
mkEqPred = mkPrimEqPred

mkHEqPred :: Type -> Type -> Type
mkHEqPred t1 t2 = TyConApp heqTyCon [typeKind t1, typeKind t2, t1, t2]
#endif

-- | Make up evidence for a fake equality constraint @t1 ~~ t2@ by
-- coercing bogus evidence of type @t1 ~ t2@ (or its heterogeneous
-- variant, in GHC 8.0).
mkFunnyEqEvidence :: Type -> Type -> Type -> EvTerm
#if __GLASGOW_HASKELL__ >= 800
mkFunnyEqEvidence t t1 t2 = EvDFunApp (dataConWrapId heqDataCon) [typeKind t1, typeKind t2, t1, t2] [evByFiat "units" t1 t2]
                       `EvCast` mkUnivCo (PluginProv "units") Representational (mkHEqPred t1 t2) t
#else
mkFunnyEqEvidence t t1 t2 = evByFiat "units" t1 t2
                       `EvCast` TcCoercion (mkUnivCo (fsLit "units") Representational (mkTyConApp eqTyCon [typeKind t1, t1, t2]) t)
#endif
