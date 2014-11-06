module Data.UnitsOfMeasure.Plugin.Convert
  ( UnitDefs(..)
  , isUnitKind
  , normaliseUnit
  , reifyUnit
  ) where

import DataCon
import TyCon
import Type
import TypeRep
import TcType

import Control.Applicative
import Data.List

import Data.UnitsOfMeasure.Plugin.NormalForm


isUnitKind :: UnitDefs -> Type -> Bool
isUnitKind uds ty | Just (tc, _) <- tcSplitTyConApp_maybe ty = tc == unitKindCon uds
                  | otherwise                                = False


normaliseUnit :: UnitDefs -> Type -> Maybe NormUnit
normaliseUnit uds ty | Just ty1 <- tcView ty = normaliseUnit uds ty1
normaliseUnit _   (TyVarTy v)                    = pure $ atom $ VarAtom v
normaliseUnit uds (TyConApp tc tys)
  | tc == promoteDataCon (unitOneDataCon uds)                 = pure one
  | tc == promoteDataCon (unitBaseDataCon uds), [x]    <- tys = atom . BaseAtom <$> isStrLitTy x
  | tc == mulTyCon uds,    [u, v] <- tys = (*:) <$> normaliseUnit uds u <*> normaliseUnit uds v
  | tc == divTyCon uds,    [u, v] <- tys = (/:) <$> normaliseUnit uds u <*> normaliseUnit uds v
  | tc == expTyCon uds,    [u, n] <- tys = (^:) <$> normaliseUnit uds u <*> isNumLitTy n
  | isFamilyTyCon tc                             = pure $ atom $ FamAtom tc tys
normaliseUnit _ _ = Nothing


reifyUnit :: UnitDefs -> NormUnit -> Type
reifyUnit uds u | null xs && null ys = oneTy
                | null ys            = foldr1 times xs
                | null xs            = oneTy `divide` foldr1 times ys
                | otherwise          = foldr1 times xs `divide` foldr1 times ys
  where
    (pos, neg) = partition ((> 0) . snd) $ ascending u
    xs = map fromAtom            pos
    ys = map (fromAtom . fmap negate) neg

    oneTy      = mkTyConApp (promoteDataCon $ unitOneDataCon uds) []
    times  x y = mkTyConApp (mulTyCon uds) [x, y]
    divide x y = mkTyConApp (divTyCon uds) [x, y]

    fromAtom (a, n) = pow n (reifyAtom uds a)
    pow 1 ty = ty
    pow n ty = mkTyConApp (expTyCon uds) [ty, mkNumLitTy n]

reifyAtom :: UnitDefs -> Atom -> Type
reifyAtom uds (BaseAtom s)    = mkTyConApp (promoteDataCon (unitBaseDataCon uds)) [mkStrLitTy s]
reifyAtom _   (VarAtom  v)    = mkTyVarTy  v
reifyAtom _   (FamAtom f tys) = mkTyConApp f tys


data UnitDefs = UnitDefs
    { unitKind        :: Kind
    , unitKindCon     :: TyCon
    , unitOneDataCon  :: DataCon
    , unitBaseDataCon :: DataCon
    , mulTyCon        :: TyCon
    , divTyCon        :: TyCon
    , expTyCon        :: TyCon
    }
