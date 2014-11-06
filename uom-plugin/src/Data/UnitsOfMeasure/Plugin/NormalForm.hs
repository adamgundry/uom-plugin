module Data.UnitsOfMeasure.Plugin.NormalForm
  ( Atom(..)
  , BaseUnit
  , NormUnit
  , isBase
  , one
  , atom
  , invariant
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
type NormUnit = Map.Map Atom Integer

isBase :: Atom -> Bool
isBase (BaseAtom _) = True
isBase _            = False

one :: NormUnit
one = Map.empty

atom :: Atom -> NormUnit
atom a = Map.singleton a 1

invariant :: NormUnit -> NormUnit
invariant = Map.filter (/= 0)

(*:) :: NormUnit -> NormUnit -> NormUnit
u *: v = invariant $ Map.unionWith (+) u v

(/:) :: NormUnit -> NormUnit -> NormUnit
u /: v = u *: invert v

(^:) :: NormUnit -> Integer -> NormUnit
_ ^: 0 = one
u ^: n = Map.map (* n) u

infixl 7 *:, /:
infixr 8 ^:

invert :: NormUnit -> NormUnit
invert = Map.map negate

isOne :: NormUnit -> Bool
isOne = Map.null

isConstant :: NormUnit -> Bool
isConstant = all isBase . Map.keys

-- | View a unit as a list of atoms in order of ascending absolute exponent
ascending :: NormUnit -> [(Atom, Integer)]
ascending = sortBy (comparing (abs . snd)) . Map.toList

-- | Drop a variable from a unit
leftover :: TyVar -> NormUnit -> NormUnit
leftover a = Map.filterWithKey (\ b _ -> VarAtom a /= b)

divisible :: Integer -> NormUnit -> Bool
divisible i = Foldable.all (\ j -> j `rem` i == 0)

divideExponents :: Integer -> NormUnit -> NormUnit
divideExponents i = invariant . Map.map (`quot` i)

-- | Substitute v for a^i in u
substUnit :: (TyVar, Integer) -> NormUnit -> NormUnit -> NormUnit
substUnit (a, i) v u = (v ^: i) *: leftover a u



cancel :: NormUnit -> NormUnit -> (NormUnit, NormUnit)
cancel u v = (u', v')
  where
    ns = Map.elems (u `Map.union` v)
    g  = foldr1 gcd ns
    ok = not (null ns) && g > 1

    (u', v') | ok        = (Map.map (`div` g) u, Map.map (`div` g) v)
             | otherwise = (u, v)
