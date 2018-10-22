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

#if __GLASGOW_HASKELL__ >= 806
evDFunApp' :: DFunId -> [Type] -> [EvExpr] -> EvTerm
evDFunApp' x ks ts =
    evDFunApp x ks ts
#elif __GLASGOW_HASKELL__ >= 802
evDFunApp' :: DFunId -> [Type] -> [EvTerm] -> EvTerm
evDFunApp' x ks ts =
    EvDFunApp x ks ts
#else
evDFunApp' :: DFunId -> [Type] -> [EvTerm] -> EvTerm
evDFunApp' x ks ts =
    EvDFunApp x ks ts
#endif

#if __GLASGOW_HASKELL__ >= 806
evCast' :: EvExpr -> TcCoercion -> EvTerm
evCast' = evCast
#elif __GLASGOW_HASKELL__ >= 802
evCast' :: EvTerm -> TcCoercion -> EvTerm
evCast' = EvCast
#else
evCast' :: EvTerm -> TcCoercionR -> EvTerm
evCast' = EvCast
#endif

-- | Make up evidence for a fake equality constraint @t1 ~~ t2@ by
-- coercing bogus evidence of type @t1 ~ t2@ (or its heterogeneous
-- variant, in GHC 8.0).
#if __GLASGOW_HASKELL__ >= 806
mkFunnyEqEvidence :: Type -> Type -> Type -> EvTerm
mkFunnyEqEvidence t t1 t2 =
    let (EvExpr e) = castFrom in e `evCast'` castTo
    where
        funId :: Id
        funId = dataConWrapId heqDataCon

        tys :: [Kind]
        tys = [typeKind t1, typeKind t2, t1, t2]

        terms :: [EvExpr]
        terms = [let (EvExpr e) = evByFiat "units" t1 t2 in e]

        from :: UnivCoProvenance
        from = PluginProv "units"

        role :: Role
        role = Representational

        tySource :: Type
        tySource = mkHEqPred t1 t2

        castFrom :: EvTerm
        castFrom = evDFunApp' funId tys terms

        castTo :: Coercion
        castTo = mkUnivCo from role tySource t
#elif __GLASGOW_HASKELL__ >= 800
mkFunnyEqEvidence :: Type -> Type -> Type -> EvTerm
mkFunnyEqEvidence t t1 t2 =
    castFrom `evCast'` castTo
    where
        funId :: Id
        funId = dataConWrapId heqDataCon

        tys :: [Kind]
        tys = [typeKind t1, typeKind t2, t1, t2]

        terms :: [EvTerm]
        terms = [evByFiat "units" t1 t2]

        from :: UnivCoProvenance
        from = PluginProv "units"

        role :: Role
        role = Representational

        tySource :: Type
        tySource = mkHEqPred t1 t2

        castFrom :: EvTerm
        castFrom = evDFunApp' funId tys terms

        castTo :: Coercion
        castTo = mkUnivCo from role tySource t
#else
mkFunnyEqEvidence :: Type -> Type -> Type -> EvTerm
mkFunnyEqEvidence t t1 t2 =
    evByFiat "units" t1 t2
    `evCast'`
    TcCoercion
        (mkUnivCo
            (fsLit "units")
            Representational
            (mkTyConApp eqTyCon [typeKind t1, t1, t2])
            t
        )
#endif
