{-# LANGUAGE PartialTypeSignatures #-}

module GhcApi.Shim
    (
      tyVarsOfType
    , tyVarsOfTypes
    , promoteTyCon

    , mkEqPred
    , mkHEqPred

    , evCast'
    ) where

import GhcApi

tyVarsOfType :: Type -> TyCoVarSet
tyVarsOfType = tyCoVarsOfType

tyVarsOfTypes :: [Type] -> TyCoVarSet
tyVarsOfTypes = tyCoVarsOfTypes

promoteTyCon :: TyCon -> TyCon
promoteTyCon = id

mkEqPred :: Type -> Type -> Type
mkEqPred = mkPrimEqPred

mkHEqPred :: Type -> Type -> Type
mkHEqPred t1 t2 = TyConApp heqTyCon [typeKind t1, typeKind t2, t1, t2]

evCast' :: EvTerm -> TcCoercion -> EvTerm
evCast' (EvExpr e)  = evCast e
evCast' (EvTypeable{}) = error "Can't evCast (EvTypeable _ _)"
evCast' (EvFun{}) = error "Can't evCast (EvFun _ _ _ _)"
