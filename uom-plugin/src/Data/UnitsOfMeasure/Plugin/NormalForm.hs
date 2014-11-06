module Data.UnitsOfMeasure.Plugin.NormalForm
  ( Atom(..)
  , BaseUnit
  , NormUnit
    -- * Constructors
  , one
  , varUnit
  , baseUnit
  , famUnit
  , mkNormUnit

    -- * Algebraic operations
  , (*:)
  , (/:)
  , (^:)
  , invert

    -- * Predicates
  , isOne
  , isConstant
  , isBase
  , divisible

    -- * Destructors
  , ascending
  , leftover
  , divideExponents
  , substUnit
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


-- | Base units are just represented as strings, for simplicity
type BaseUnit = FastString

-- | An atom in the normal form is either a base unit, a variable or a
-- stuck type family application (but not one of the built-in type
-- families that correspond to group operations).
data Atom = BaseAtom BaseUnit | VarAtom TyVar | FamAtom TyCon [Type]

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


-- | A unit normal form is a signed multiset of atoms; we maintain the
-- invariant that the map does not contain any zero values.
newtype NormUnit = NormUnit { _NormUnit :: Map.Map Atom Integer }

instance Outputable NormUnit where
    ppr = ppr . Map.map show . _NormUnit


-- | The group identity, representing the dimensionless unit
one :: NormUnit
one = NormUnit Map.empty

-- | Construct a normalised unit from an atom
atom :: Atom -> NormUnit
atom a = NormUnit $ Map.singleton a 1

-- | Construct a normalised unit from a single variable
varUnit :: TyVar -> NormUnit
varUnit = atom . VarAtom

-- | Construct a normalised unit from a single base unit
baseUnit :: BaseUnit -> NormUnit
baseUnit = atom . BaseAtom

-- | Construct a normalised unit from a stuck type family application:
-- this must not be one of the built-in type families!
famUnit :: TyCon -> [Type] -> NormUnit
famUnit tc = atom . FamAtom tc

-- | Construct a normalised unit from a list of atom-exponent pairs
mkNormUnit :: [(Atom, Integer)] -> NormUnit
mkNormUnit = mkNormUnitMap . Map.fromList

-- | Construct a normalised unit from an atom-exponent map, applying
-- the signed multiset invariant
mkNormUnitMap :: Map.Map Atom Integer -> NormUnit
mkNormUnitMap =  NormUnit . Map.filter (/= 0)


-- | Multiplication of normalised units
(*:) :: NormUnit -> NormUnit -> NormUnit
u *: v = mkNormUnitMap $ Map.unionWith (+) (_NormUnit u) (_NormUnit v)

-- | Division of normalised units
(/:) :: NormUnit -> NormUnit -> NormUnit
u /: v = u *: invert v

-- | Expontentiation of normalised units
(^:) :: NormUnit -> Integer -> NormUnit
_ ^: 0 = one
u ^: n = NormUnit $ Map.map (* n) $ _NormUnit u

infixl 7 *:, /:
infixr 8 ^:

-- | Invert a normalised unit
invert :: NormUnit -> NormUnit
invert = NormUnit . Map.map negate . _NormUnit


-- | Test whether a unit is dimensionless
isOne :: NormUnit -> Bool
isOne = Map.null . _NormUnit

-- | Test whether a unit is constant (contains only base units)
isConstant :: NormUnit -> Bool
isConstant = all isBase . Map.keys . _NormUnit

-- | Test whether an atom is a base unit
isBase :: Atom -> Bool
isBase (BaseAtom _) = True
isBase _            = False

-- | Test whether all exponents in a unit are divisble by an integer
divisible :: Integer -> NormUnit -> Bool
divisible i = Foldable.all (\ j -> j `rem` i == 0) . _NormUnit


-- | View a unit as a list of atoms in order of ascending absolute exponent
ascending :: NormUnit -> [(Atom, Integer)]
ascending = sortBy (comparing (abs . snd)) . Map.toList . _NormUnit

-- | Drop a variable from a unit
leftover :: TyVar -> NormUnit -> NormUnit
leftover a = NormUnit . Map.delete (VarAtom a) . _NormUnit

-- | Divide all the exponents in a unit by an integer
divideExponents :: Integer -> NormUnit -> NormUnit
divideExponents i = mkNormUnitMap . Map.map (`quot` i) . _NormUnit

-- | Substitute the first unit for the variable in the second unit
substUnit :: TyVar -> NormUnit -> NormUnit -> NormUnit
substUnit a v u = case Map.lookup (VarAtom a) $ _NormUnit u of
                    Nothing -> u
                    Just i  -> (v ^: i) *: leftover a u
