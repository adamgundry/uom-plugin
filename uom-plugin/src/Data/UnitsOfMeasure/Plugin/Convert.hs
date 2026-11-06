{-# LANGUAGE CPP #-}

module Data.UnitsOfMeasure.Plugin.Convert
  ( UnitDefs(..)
  , unitKind
  , unitKindCoercion
  , normaliseUnit
  , reifyUnit
  , reifyUnitUnpacked
  ) where

import GhcApi (Type(..), tcSplitTyConApp_maybe, coreView, isFamilyTyCon, MCoercion(MRefl), MCoercionN, coToMCo, mkTransMCo, mkCastTyMCo, FamInstEnvs, normaliseType)
import GHC.Builtins (typeSymbolKind, nilDataCon, consDataCon)
import Data.List

import GHC.TcPlugin.API

import Data.UnitsOfMeasure.Plugin.NormalForm

-- | Contains references to the basic unit constructors declared in
-- "Data.UnitsOfMeasure", as loaded inside GHC.
data UnitDefs = UnitDefs
    { unitKindCon   :: TyCon -- ^ The 'Unit' type constructor, to be promoted to a kind
    , unitBaseTyCon :: TyCon -- ^ The 'Base' type family
    , unitOneTyCon  :: TyCon -- ^ The 'One' data constructor of 'Unit', promoted to a type constructor
    , mulTyCon      :: TyCon -- ^ The '(*:)' type family
    , divTyCon      :: TyCon -- ^ The '(/:)' type family
    , expTyCon      :: TyCon -- ^ The '(^:)' type family
    , unpackTyCon     :: TyCon -- ^ The 'Unpack' type family
    , unitSyntaxTyCon :: TyCon -- ^ The 'UnitSyntax' type constructor, to be promoted to a kind
    , unitSyntaxPromotedDataCon :: TyCon -- ^ The data constructor of 'UnitSyntax', promoted to a type constructor
    , equivTyCon      :: TyCon -- ^ The '(~~)' type family
    }

-- | 'Unit' promoted to a kind
unitKind :: UnitDefs -> Kind
unitKind uds = TyConApp (unitKindCon uds) []

-- | Is this the 'Unit' kind?
isUnitKind :: UnitDefs -> Kind -> Bool
isUnitKind uds ty | Just (tc, _) <- tcSplitTyConApp_maybe ty = tc == unitKindCon uds
                  | otherwise                                = False

-- | If the kind reduces to 'Unit', return a coercion from the kind to 'Unit'.
unitKindCoercion :: UnitDefs -> FamInstEnvs -> Kind -> Maybe MCoercionN
unitKindCoercion uds famInstEnvs kind
  | isUnitKind uds reduced = Just (coToMCo co)
  | otherwise              = Nothing
  where
#if MIN_VERSION_ghc(9,4,0)
    Reduction co reduced = normaliseType famInstEnvs Nominal kind
#else
    (co, reduced) = normaliseType famInstEnvs Nominal kind
#endif


-- | Try to convert a type to a unit normal form.
--
-- Fails if the type does not have kind 'Unit' or cannot be put into normal form.
normaliseUnit :: UnitDefs -> Type -> Maybe NormUnit
normaliseUnit uds ty0
  | isUnitKind uds (typeKind ty0) = go MRefl ty0
  | otherwise                     = Nothing
  where
    go :: MCoercionN -> Type -> Maybe NormUnit
    go co ty | Just ty1 <- coreView ty = go co ty1
    go co (CastTy ty co')              = go (coToMCo co' `mkTransMCo` co) ty
    go co (TyVarTy v)                  = pure $ varUnit v co
    go co (TyConApp tc tys)
      | tc == unitOneTyCon  uds                = pure one
      | tc == unitBaseTyCon uds, [x]    <- tys = pure $ baseUnit x
      | tc == mulTyCon      uds, [u, v] <- tys = (*:) <$> go MRefl u <*> go MRefl v
      | tc == divTyCon      uds, [u, v] <- tys = (/:) <$> go MRefl u <*> go MRefl v
      | tc == expTyCon      uds, [u, n] <- tys, Just i <- isNumLitTy n = (^:) <$> go MRefl u <*> pure i
      | isFamilyTyCon tc                       = pure $ famUnit tc tys co
    go _ _ = Nothing


-- | Convert a unit normal form to a type expression of kind 'Unit'
reifyUnit :: UnitDefs -> NormUnit -> Type
reifyUnit uds u | null xs && null ys = oneTy
                | null ys            = foldr1 times xs
                | null xs            = oneTy `divide` foldr1 times ys
                | otherwise          = foldr1 times xs `divide` foldr1 times ys
  where
    (pos, neg) = partition ((> 0) . snd) $ ascending u
    xs = map fromAtom            pos
    ys = map (fromAtom . fmap negate) neg

    oneTy      = mkTyConApp (unitOneTyCon uds) []
    times  x y = mkTyConApp (mulTyCon uds) [x, y]
    divide x y = mkTyConApp (divTyCon uds) [x, y]

    fromAtom (a, n) = pow n (reifyAtom a)
    pow 1 ty = ty
    pow n ty = mkTyConApp (expTyCon uds) [ty, mkNumLitTy n]

    reifyAtom (BaseAtom s)       = mkTyConApp (unitBaseTyCon uds) [s]
    reifyAtom (VarAtom  v co)    = mkCastTyMCo (mkTyVarTy v)      co
    reifyAtom (FamAtom f tys co) = mkCastTyMCo (mkTyConApp f tys) co


-- | Convert a constant unit normal form into a type expression of kind
-- @UnitSyntax Symbol@.
reifyUnitUnpacked  :: UnitDefs -> [(BaseUnit, Integer)] -> Type
reifyUnitUnpacked uds xs =
  mkTyConApp (unitSyntaxPromotedDataCon uds)
             [ typeSymbolKind
             , foldr promoter nil ys
             , foldr promoter nil zs
             ]
  where
    ys = concatMap (\ (s, i) -> if i > 0 then genericReplicate i s       else []) xs
    zs = concatMap (\ (s, i) -> if i < 0 then genericReplicate (abs i) s else []) xs

    nil = mkTyConApp (promoteDataCon nilDataCon) [typeSymbolKind]

    promoter x t = mkTyConApp cons_tycon [typeSymbolKind, mkStrLitTy x, t]
    cons_tycon = promoteDataCon consDataCon
