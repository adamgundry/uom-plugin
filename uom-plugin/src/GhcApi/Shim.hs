{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ > 710
{-# LANGUAGE PatternSynonyms #-}
#endif

module GhcApi.Shim where

import GhcApi

#if __GLASGOW_HASKELL__ > 710
tyVarsOfType :: Type -> TyCoVarSet
tyVarsOfType = tyCoVarsOfType

tyVarsOfTypes :: [Type] -> TyCoVarSet
tyVarsOfTypes = tyCoVarsOfTypes

promoteTyCon :: TyCon -> TyCon
promoteTyCon = id
#endif

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
