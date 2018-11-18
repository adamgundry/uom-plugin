module Data.UnitsOfMeasure.Plugin.Convert
  ( UnitDefs(..)
  , unitKind
  , isUnitKind
  , normaliseUnit
  , reifyUnit
  ) where

import GhcApi
import GhcApi.Shim (promoteTyCon)
import Data.List

import Data.UnitsOfMeasure.Plugin.NormalForm

-- | Contains references to the basic unit constructors declared in
-- "Data.UnitsOfMeasure", as loaded inside GHC.
data UnitDefs = UnitDefs
    { unitKindCon   :: TyCon -- ^ The 'Unit' type constructor, to be promoted to a kind
    , unitBaseTyCon :: TyCon -- ^ The 'Base' data constructor of 'Unit', promoted to a type constructor
    , unitOneTyCon  :: TyCon -- ^ The 'One'  type family
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
unitKind uds = TyConApp (promoteTyCon $ unitKindCon uds) []

-- | Is this the 'Unit' kind?
isUnitKind :: UnitDefs -> Kind -> Bool
isUnitKind uds ty | Just (tc, _) <- tcSplitTyConApp_maybe ty = tc == unitKindCon uds
                  | otherwise                                = False


-- | Try to convert a type to a unit normal form; this does not check
-- the type has kind 'Unit', and may fail even if it does.
normaliseUnit :: UnitDefs -> Type -> Maybe NormUnit
normaliseUnit uds ty | Just ty1 <- coreView ty = normaliseUnit uds ty1
normaliseUnit _   (TyVarTy v)              = pure $ varUnit v
normaliseUnit uds (TyConApp tc tys)
  | tc == unitOneTyCon  uds                = pure one
  | tc == unitBaseTyCon uds, [x]    <- tys = pure $ baseUnit x
  | tc == mulTyCon      uds, [u, v] <- tys = (*:) <$> normaliseUnit uds u <*> normaliseUnit uds v
  | tc == divTyCon      uds, [u, v] <- tys = (/:) <$> normaliseUnit uds u <*> normaliseUnit uds v
  | tc == expTyCon      uds, [u, n] <- tys, Just i <- isNumLitTy n = (^:) <$> normaliseUnit uds u <*> pure i
  | isFamilyTyCon tc                       = pure $ famUnit tc tys
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

    reifyAtom (BaseAtom s)    = mkTyConApp (unitBaseTyCon uds) [s]
    reifyAtom (VarAtom  v)    = mkTyVarTy  v
    reifyAtom (FamAtom f tys) = mkTyConApp f tys
