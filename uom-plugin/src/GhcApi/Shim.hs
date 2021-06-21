{-# LANGUAGE CPP #-}
{-# LANGUAGE PartialTypeSignatures #-}

module GhcApi.Shim
    (
      tyVarsOfType
    , tyVarsOfTypes
    , promoteTyCon

    , mkEqPred
    , mkHEqPred
    , mkFunnyEqEvidence
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

evDFunApp' :: DFunId -> [Type] -> [EvExpr] -> EvTerm
evDFunApp' = evDFunApp

evCast' :: EvTerm -> TcCoercion -> EvTerm
evCast' (EvExpr e)  = evCast e
evCast' (EvTypeable{}) = error "Can't evCast (EvTypeable _ _)"
evCast' (EvFun{}) = error "Can't evCast (EvFun _ _ _ _)"

-- | Make up evidence for a fake equality constraint @t1 ~~ t2@ by coercing
-- bogus evidence of type @t1 ~ t2@.
mkFunnyEqEvidence :: Type -> Type -> Type -> EvTerm
mkFunnyEqEvidence t t1 t2 =
    castFrom `evCast'` castTo
    where
        castFrom :: EvTerm
        castFrom = evDFunApp' funId tys terms
            where
                funId :: Id
                funId = dataConWrapId heqDataCon

                tys :: [Kind]
                tys = [typeKind t1, typeKind t2, t1, t2]

                terms :: [EvExpr]
                terms = case evByFiat "units" t1 t2 of
                          EvExpr e -> [e]
                          _        -> error "evByFiat isn't an EvExpr?"

        castTo :: TcCoercion
        castTo =
            mkUnivCo from Representational tySource t
            where
                from :: UnivCoProvenance
                from = PluginProv "units"

                tySource :: Type
                tySource = mkHEqPred t1 t2
