module Data.UnitsOfMeasure.Plugin.NormalForm
  ( Atom(..)
  , BaseUnit
  , NormUnit
  , isBase
  , one
  , atom
  , mkNormUnit
  , (*:)
  , (/:)
  , (^:)
  , invert
  , isOne
  , isConstant
  , ascending
  , leftover
  , divisible
  , divideExponents
  , substUnit
  , cancel
  ) where

import Type
import TyCon

import FastString
import Outputable
import Util ( thenCmp )


import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import Data.List ( sortBy )
import Data.Ord



data Atom     = BaseAtom BaseUnit | VarAtom TyVar | FamAtom TyCon [Type]

instance Eq Atom where
  a == b = compare a b == EQ

-- TODO: using cmpTypes here probably isn't ideal, but does it matter?
instance Ord Atom where
  compare (BaseAtom x)    (BaseAtom y)      = compare x y
  compare (BaseAtom _)    _                 = LT
  compare (VarAtom  _)    (BaseAtom _)      = GT
  compare (VarAtom  a)    (VarAtom  b)      = compare a b
  compare (VarAtom  _)    (FamAtom _ _)     = LT
  compare (FamAtom f tys) (FamAtom f' tys') = compare f f' `thenCmp` cmpTypes tys tys'
  compare (FamAtom _ _)   _                 = GT

instance Outputable Atom where
  ppr (BaseAtom b) = ppr b
  ppr (VarAtom  v) = ppr v
  ppr (FamAtom tc tys) = ppr tc <> text " " <> ppr tys

type BaseUnit = FastString
newtype NormUnit = NormUnit { _NormUnit :: Map.Map Atom Integer }

instance Outputable NormUnit where
    ppr = ppr . Map.map show . _NormUnit


isBase :: Atom -> Bool
isBase (BaseAtom _) = True
isBase _            = False

one :: NormUnit
one = NormUnit Map.empty

atom :: Atom -> NormUnit
atom a = NormUnit $ Map.singleton a 1

mkNormUnit :: [(Atom, Integer)] -> NormUnit
mkNormUnit = invariant . NormUnit . Map.fromList

invariant :: NormUnit -> NormUnit
invariant =  NormUnit . Map.filter (/= 0) . _NormUnit

(*:) :: NormUnit -> NormUnit -> NormUnit
u *: v = invariant $ NormUnit $ Map.unionWith (+) (_NormUnit u) (_NormUnit v)

(/:) :: NormUnit -> NormUnit -> NormUnit
u /: v = u *: invert v

(^:) :: NormUnit -> Integer -> NormUnit
_ ^: 0 = one
u ^: n = NormUnit $ Map.map (* n) $ _NormUnit u

infixl 7 *:, /:
infixr 8 ^:

invert :: NormUnit -> NormUnit
invert = NormUnit . Map.map negate . _NormUnit

isOne :: NormUnit -> Bool
isOne = Map.null . _NormUnit

isConstant :: NormUnit -> Bool
isConstant = all isBase . Map.keys . _NormUnit

-- | View a unit as a list of atoms in order of ascending absolute exponent
ascending :: NormUnit -> [(Atom, Integer)]
ascending = sortBy (comparing (abs . snd)) . Map.toList . _NormUnit

-- | Drop a variable from a unit
leftover :: TyVar -> NormUnit -> NormUnit
leftover a = NormUnit . Map.filterWithKey (\ b _ -> VarAtom a /= b) . _NormUnit

divisible :: Integer -> NormUnit -> Bool
divisible i = Foldable.all (\ j -> j `rem` i == 0) . _NormUnit

divideExponents :: Integer -> NormUnit -> NormUnit
divideExponents i = invariant . NormUnit . Map.map (`quot` i) . _NormUnit

-- | Substitute v for a in u
substUnit :: TyVar -> NormUnit -> NormUnit -> NormUnit
substUnit a v u = case Map.lookup (VarAtom a) $ _NormUnit u of
                    Nothing -> u
                    Just i  -> (v ^: i) *: leftover a u



cancel :: NormUnit -> NormUnit -> (NormUnit, NormUnit)
cancel (NormUnit u) (NormUnit v) = (NormUnit u', NormUnit v')
  where
    ns = Map.elems (u `Map.union` v)
    g  = foldr1 gcd ns
    ok = not (null ns) && g > 1

    (u', v') | ok        = (Map.map (`div` g) u, Map.map (`div` g) v)
             | otherwise = (u, v)
