{-# LANGUAGE PatternSynonyms #-}

module GHC.Corroborate.Type (collectType) where

import TyCoRep (Type(..), TyBinder(Anon))

import Data.UnitsOfMeasure.Unsafe.Convert (UnitDefs, eqTc, collectKindOrType)
import Data.UnitsOfMeasure.Unsafe.NormalForm (BaseUnit)

pattern FunTy :: Type -> Type -> Type
pattern FunTy t v = ForAllTy (Anon t) v

collectType :: UnitDefs -> a -> Type -> [(a, Type, [(BaseUnit, Integer)])]
collectType uds ct (AppTy f s) = collectType uds ct f ++ collectType uds ct s
collectType uds ct (TyConApp tc [a]) | eqTc uds tc = collectKindOrType uds ct a
collectType uds ct (TyConApp _ as) = concatMap (collectType uds ct) as
collectType uds ct (FunTy t v) = collectType uds ct t ++ collectType uds ct v
collectType uds ct (ForAllTy _ t) = collectType uds ct t
collectType _ _ TyVarTy{} = []
collectType _ _ LitTy{} = []
collectType _ _ CastTy{} = []
collectType _ _ CoercionTy{} = []
