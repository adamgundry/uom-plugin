module Data.UnitsOfMeasure.Plugin.Convert
  ( UnitDefs(..)
  , unitKind
  , isUnitKind
  , normaliseUnit
  , reifyUnit
  , reifyUnitUnpacked
  ) where

import GhcApi (Type(..), nilDataCon, consDataCon, coreView, tcSplitTyConApp_maybe, isFamilyTyCon)
import Data.List

import GHC.TcPlugin.API
import GHC.Core.Type (irrelevantMult)
import GHC.Core.TyCon (isAlgTyCon)
import GHC.Tc.Utils.TcType (tcSplitFunTy_maybe)

import Data.UnitsOfMeasure.Plugin.NormalForm

-- | Contains references to the basic unit constructors declared in
-- "Data.UnitsOfMeasure", as loaded inside GHC.
data UnitDefs = UnitDefs
    { unitTagKindCon            :: TyCon -- ^ The 'UnitKind' type constructor
    , unitTypeSyn               :: TyCon -- ^ The 'Unit' type synonym
    , unitBaseTyCon             :: TyCon -- ^ The 'Base' type family
    , unitOneTyCon              :: TyCon -- ^ The 'One'  type family
    , mulTyCon                  :: TyCon -- ^ The '(*:)' type family
    , divTyCon                  :: TyCon -- ^ The '(/:)' type family
    , expTyCon                  :: TyCon -- ^ The '(^:)' type family
    , unpackTyCon               :: TyCon -- ^ The 'Unpack' type family
    , unitSyntaxTyCon           :: TyCon -- ^ The 'UnitSyntax' type constructor, to be promoted to a kind
    , unitSyntaxPromotedDataCon :: TyCon -- ^ The data constructor of 'UnitSyntax', promoted to a type constructor
    , equivTyCon                :: TyCon -- ^ The '(~~)' type family
    }

-- | 'Unit' promoted to a kind
unitKind :: UnitDefs -> Kind
unitKind uds = mkTyConApp (unitTypeSyn uds) []

-- | Is this the 'Unit' kind?
isUnitKind :: UnitDefs -> Kind -> Bool
isUnitKind uds ty
  | Just (sty, _) <- tcSplitFunTy_maybe ty
  , let ty' = irrelevantMult sty
  , Just (tc, _) <- tcSplitTyConApp_maybe ty' = tc == unitTagKindCon uds
  | otherwise                                 = False


-- | Try to convert a type to a unit normal form; this does not check
-- the type has kind 'Unit', and may fail even if it does.
normaliseUnit :: UnitDefs -> Type -> Maybe NormUnit
normaliseUnit uds ty | Just ty1 <- coreView ty = normaliseUnit uds ty1
normaliseUnit _   (TyVarTy v)              = pure $ varUnit v
normaliseUnit uds ty@(TyConApp tc tys)
  | tc == unitOneTyCon  uds                = pure one
-- | tc == unitBaseTyCon uds, [x]    <- tys = pure $ baseUnit x
  | tc == mulTyCon      uds, [u, v] <- tys = (*:) <$> normaliseUnit uds u <*> normaliseUnit uds v
  | tc == divTyCon      uds, [u, v] <- tys = (/:) <$> normaliseUnit uds u <*> normaliseUnit uds v
  | tc == expTyCon      uds, [u, n] <- tys, Just i <- isNumLitTy n = (^:) <$> normaliseUnit uds u <*> pure i
  | isFamilyTyCon tc                       = pure $ famUnit tc tys
  | isAlgTyCon tc, null tys = pure $ baseUnit ty
normaliseUnit _ _ = Nothing


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

    reifyAtom (BaseAtom s)    = s -- mkTyConApp (unitBaseTyCon uds) [s]
    reifyAtom (VarAtom  v)    = mkTyVarTy  v
    reifyAtom (FamAtom f tys) = mkTyConApp f tys


-- | Convert a constant unit normal form into a type expression of kind
-- @UnitSyntax Symbol@.
reifyUnitUnpacked  :: UnitDefs -> [(BaseUnit, Integer)] -> Type
reifyUnitUnpacked uds xs =
  mkTyConApp (unitSyntaxPromotedDataCon uds)
             [ baseUnitKind
             , foldr promoter nil ys
             , foldr promoter nil zs
             ]
  where
    ys = concatMap (\ (s, i) -> if i > 0 then genericReplicate i s       else []) xs
    zs = concatMap (\ (s, i) -> if i < 0 then genericReplicate (abs i) s else []) xs

    nil = mkTyConApp (promoteDataCon nilDataCon) [baseUnitKind]

    promoter x t = mkTyConApp cons_tycon [baseUnitKind, baseUnitToUnit x, t]
    cons_tycon = promoteDataCon consDataCon

    baseUnitKind = unitKind uds
    baseUnitToUnit tc = mkTyConApp tc []
