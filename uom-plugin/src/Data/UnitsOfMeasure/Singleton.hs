{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines singleton types for integers and concrete
-- units.
module Data.UnitsOfMeasure.Singleton
    ( -- * Singletons for units
      SUnit(..)
    , forgetSUnit
    , KnownUnit(..)
    , unitVal
    , testEquivalentSUnit

      -- * Singletons for lists
    , SList(..)
    , KnownList(..)
    ) where

import GHC.TypeLits
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Type.Equality
import Unsafe.Coerce

import Data.UnitsOfMeasure.Internal


-- | Singleton type for concrete units of measure represented as lists
-- of base units
data SUnit (u :: UnitSyntax Symbol) where
  SUnit :: SList xs -> SList ys -> SUnit (xs :/ ys)

-- | Singleton type for lists of base units
data SList (xs :: [Symbol]) where
  SNil :: SList '[]
  SCons :: KnownSymbol x => proxy x -> SList xs -> SList (x ': xs)

instance TestEquality SUnit where
  testEquality (SUnit xs ys) (SUnit xs' ys') = case (testEquality xs xs', testEquality ys ys') of
                                                 (Just Refl, Just Refl) -> Just Refl
                                                 _                      -> Nothing

instance TestEquality SList where
  testEquality SNil SNil = Just Refl
  testEquality (SCons px xs) (SCons py ys)
    | Just Refl <- testEqualitySymbol px py
    , Just Refl <- testEquality xs ys = Just Refl
  testEquality _ _ = Nothing

-- | Annoyingly, base doesn't appear to export enough stuff to make it
-- possible to write a @TestEquality SSymbol@ instance, so we cheat.
testEqualitySymbol :: forall proxy proxy' x y . (KnownSymbol x, KnownSymbol y)
                   => proxy x -> proxy' y -> Maybe (x :~: y)
testEqualitySymbol px py
  | symbolVal px == symbolVal py = Just (unsafeCoerce Refl)
  | otherwise                    = Nothing

-- | Test whether two 'SUnit's represent the same units, up to the
-- equivalence relation.  TODO: this currently uses 'unsafeCoerce',
-- but in principle it should be possible to avoid it.
testEquivalentSUnit :: SUnit u -> SUnit v -> Maybe (Pack u :~: Pack v)
testEquivalentSUnit su sv
  | normaliseUnitSyntax (forgetSUnit su) == normaliseUnitSyntax (forgetSUnit sv) = Just (unsafeCoerce Refl)
  | otherwise = Nothing

-- | Calculate a normal form of a syntactic unit: a map from base unit names to
-- non-zero integers.
--
-- >>> normaliseUnitSyntax ([] :/ [])
-- fromList []
--
-- >>> normaliseUnitSyntax (["m"] :/ [])
-- fromList [("m",1)]
--
-- >>> normaliseUnitSyntax (["m", "m"] :/ [])
-- fromList [("m",2)]
--
-- >>> normaliseUnitSyntax (["m", "m", "m"] :/ [])
-- fromList [("m",3)]
--
-- >>> normaliseUnitSyntax ([] :/ ["m"])
-- fromList [("m",1)]
--
-- >>> normaliseUnitSyntax ([] :/ ["m", "m"])
-- fromList []
--
-- >>> normaliseUnitSyntax ([] :/ ["m", "m", "m"])
-- fromList [("m",1)]
--
-- >>> normaliseUnitSyntax (["m"] :/ ["m"])
-- fromList []
--
-- >>> normaliseUnitSyntax (["m", "m"] :/ ["m"])
-- fromList [("m",-1)]
--
-- >>> normaliseUnitSyntax (["m"] :/ ["m", "m"])
-- fromList [("m",1)]
--
-- >>> normaliseUnitSyntax (["m", "m"] :/ ["m", "m"])
-- fromList [("m",2)]
--
-- >>> normaliseUnitSyntax (["m", "m", "m"] :/ ["m", "m"])
-- fromList [("m",3)]
--
-- >>> normaliseUnitSyntax (["m", "m"] :/ ["m", "m", "m"])
-- fromList [("m",-1)]
--
-- >>> normaliseUnitSyntax (["m", "m", "m"] :/ ["m", "m", "m"])
-- fromList [("m",-2)]
--
-- >>> normaliseUnitSyntax (replicate 3 "m" :/ [])
-- fromList [("m",3)]
--
-- >>> normaliseUnitSyntax ([] :/ replicate 3 "m")
-- fromList [("m",1)]
--
-- >>> normaliseUnitSyntax (replicate 3 "m" :/ replicate 3 "m")
-- fromList [("m",-2)]
--
-- >>> normaliseUnitSyntax (["m"] :/ ["s"])
-- fromList [("m",1),("s",1)]
--
-- >>> normaliseUnitSyntax (["m"] :/ ["s", "s"])
-- fromList [("m",1)]
--
-- >>> normaliseUnitSyntax (["m", "m"] :/ []) == normaliseUnitSyntax (["m", "m"] :/ ["m", "m"])
-- True
--
-- >>> normaliseUnitSyntax (["m"] :/ []) == normaliseUnitSyntax (["m"] :/ ["m", "m"])
-- True
normaliseUnitSyntax :: UnitSyntax String -> Map.Map String Integer
normaliseUnitSyntax (xs :/ ys) =
    Map.filter (/= 0)
        (foldl' (\ m x -> Map.insertWith (-) x 1 m)
            (foldl' (\ m x -> Map.insertWith (+) x 1 m) Map.empty xs) ys)


-- | Extract the runtime syntactic representation from a singleton unit
forgetSUnit :: SUnit u -> UnitSyntax String
forgetSUnit (SUnit xs ys) = forgetSList xs :/ forgetSList ys

forgetSList :: SList xs -> [String]
forgetSList SNil = []
forgetSList (SCons px xs) = symbolVal px : forgetSList xs


-- | A constraint @'KnownUnit' u@ means that @u@ must be a concrete
-- unit that is statically known but passed at runtime
class KnownUnit (u :: UnitSyntax Symbol) where
  unitSing :: SUnit u

instance (KnownList xs, KnownList ys) => KnownUnit (xs :/ ys) where
  unitSing = SUnit listSing listSing


-- | A constraint @'KnownList' xs@ means that @xs@ must be a list of
-- base units that is statically known but passed at runtime
class KnownList (xs :: [Symbol]) where
  listSing :: SList xs

instance KnownList '[] where
  listSing = SNil

instance (KnownSymbol x, KnownList xs) => KnownList (x ': xs) where
  listSing = SCons (undefined :: proxy x) listSing


-- | Extract the runtime syntactic representation of a 'KnownUnit'
unitVal :: forall proxy u . KnownUnit u => proxy u -> UnitSyntax String
unitVal _ = forgetSUnit (unitSing :: SUnit u)

-- $setup
-- >>> :set -XExplicitNamespaces -XDataKinds -XTypeOperators
