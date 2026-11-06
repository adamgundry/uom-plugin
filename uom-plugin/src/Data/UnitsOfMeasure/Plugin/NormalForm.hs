{-# LANGUAGE TupleSections #-}

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
  , maybeSingleVariable
  , isConstant
  , maybeConstant
  , isBase
  , divisible
  , occurs

    -- * Destructors
  , ascending
  , leftover
  , divideExponents
  , substUnit
  ) where

import Prelude hiding ((<>))
import GhcApi (elemVarSet, unionVarSet, tyCoVarsOfType, tyCoVarsOfTypes, tyCoVarsOfMCo, MCoercion(MRefl), MCoercionN, text, (<>))
import GhcApi.Compare (cmpType, cmpTypes, cmpTyCon)

import GHC.TcPlugin.API

import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import Data.List ( sortOn )
import Data.Maybe
import qualified Data.Semigroup as S

-- | Base units are just represented as strings, for simplicity
type BaseUnit = FastString

-- | An atom in the normal form.
data Atom
  -- | A base unit.
  = BaseAtom Type
  -- | A variable unit.
  | VarAtom
     TyVar      -- ^ @v@
     MCoercionN -- ^ @mco :: tyVarKind v ~# Unit@
  -- | A stuck type family application.
  --
  -- Never one of the built-in type families that correspond to group operations.
  | FamAtom
      TyCon      -- ^ @tc@
      [Type]     -- ^ @tys@
      MCoercionN -- ^ @mco :: typeKind(tc tys) ~# Unit@

instance Eq Atom where
  a == b = compare a b == EQ

-- TODO: using cmpTypes here probably isn't ideal, but does it matter?
instance Ord Atom where
  compare (BaseAtom x)      (BaseAtom y)        = cmpType x y
  compare (BaseAtom _)      _                   = LT
  compare (VarAtom  _ _)    (BaseAtom _)        = GT
  compare (VarAtom  a _)    (VarAtom  b _)      = compare a b
  compare (VarAtom  _ _)    (FamAtom _ _ _)     = LT
  compare (FamAtom f tys _) (FamAtom f' tys' _) = cmpTyCon f f' S.<> cmpTypes tys tys'
  compare (FamAtom _ _ _)   _                   = GT

instance Outputable Atom where
  ppr (BaseAtom b) = ppr b
  ppr (VarAtom v _co) = ppr v
  ppr (FamAtom tc tys _co) = ppr tc <> text " " <> ppr tys


-- | A unit normal form is a signed multiset of atoms; we maintain the
-- invariant that the map does not contain any zero values.
newtype NormUnit = NormUnit { _NormUnit :: Map.Map Atom Integer }

instance Outputable NormUnit where
    ppr = ppr . _NormUnit


-- | The group identity, representing the dimensionless unit
one :: NormUnit
one = NormUnit Map.empty

-- | Construct a normalised unit from an atom
atom :: Atom -> NormUnit
atom a = NormUnit $ Map.singleton a 1

-- | Construct a normalised unit from a single variable.
varUnit
  :: TyVar      -- ^ @v@
  -> MCoercionN -- ^ @mco :: tyVarKind v ~# Unit@
  -> NormUnit
varUnit v = atom . VarAtom v

-- | Construct a normalised unit from a single base unit
baseUnit :: Type -> NormUnit
baseUnit = atom . BaseAtom

-- | Construct a normalised unit from a stuck type family application.
--
-- This must not be one of the built-in type families!
famUnit
  :: TyCon      -- ^ @tc@
  -> [Type]     -- ^ @tys@
  -> MCoercionN -- ^ @mco :: typeKind(tc tys) ~# Unit@
  -> NormUnit
famUnit tc tys = atom . FamAtom tc tys

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

-- | Test whether a unit consists of a single variable with multiplicity 1.
maybeSingleVariable :: NormUnit -> Maybe TyVar
maybeSingleVariable x = case Map.toList (_NormUnit x) of
    [(VarAtom v _, 1)] -> Just v
    _                  -> Nothing

-- | Test whether a unit is constant (contains only base literals)
isConstant :: NormUnit -> Bool
isConstant = all isBaseLiteral . Map.keys . _NormUnit

-- | Extract the base units if a unit is constant
maybeConstant :: NormUnit -> Maybe [(BaseUnit, Integer)]
maybeConstant = mapM getBase . Map.toList . _NormUnit
  where
    getBase (BaseAtom ty, i) = (, i) <$> isStrLitTy ty
    getBase _                = Nothing

-- | Test whether an atom is a base unit (but not necessarily a
-- *literal*, e.g. it could be @Base b@ for some variable @b@)
isBase :: Atom -> Bool
isBase (BaseAtom _) = True
isBase _            = False

-- | Test whether an atom is a literal base unit
isBaseLiteral :: Atom -> Bool
isBaseLiteral (BaseAtom ty) = isJust $ isStrLitTy ty
isBaseLiteral _             = False

-- | Test whether all exponents in a unit are divisble by an integer
divisible :: Integer -> NormUnit -> Bool
divisible i = Foldable.all (\ j -> j `rem` i == 0) . _NormUnit

-- | Test whether a type variable occurs in a unit (possibly under a
-- type family application)
occurs :: TyVar -> NormUnit -> Bool
occurs a = any occursAtom . Map.keys . _NormUnit
  where
    occursAtom (BaseAtom ty)      = elemVarSet a $ tyCoVarsOfType ty
    occursAtom (VarAtom b co)     = a == b || elemVarSet a (tyCoVarsOfMCo co)
    occursAtom (FamAtom _ tys co) = elemVarSet a $ tyCoVarsOfTypes tys `unionVarSet` tyCoVarsOfMCo co


-- | View a unit as a list of atoms in order of ascending absolute exponent
ascending :: NormUnit -> [(Atom, Integer)]
ascending = sortOn (abs . snd) . Map.toList . _NormUnit

-- | Drop a variable from a unit
leftover :: TyVar -> NormUnit -> NormUnit
leftover a = NormUnit . Map.delete (VarAtom a MRefl) . _NormUnit

-- | Divide all the exponents in a unit by an integer
divideExponents :: Integer -> NormUnit -> NormUnit
divideExponents i = mkNormUnitMap . Map.map (`quot` i) . _NormUnit

-- | Substitute the first unit for the variable in the second unit
substUnit :: TyVar -> NormUnit -> NormUnit -> NormUnit
substUnit a v u = case Map.lookup (VarAtom a MRefl) $ _NormUnit u of
                    Nothing -> u
                    Just i  -> (v ^: i) *: leftover a u
