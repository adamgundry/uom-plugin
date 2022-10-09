{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeApplications #-}
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
      SBaseUnit(..)
    , KnownBaseUnit(..)

    , SUnit(..)
    , KnownUnit
    , KnownUnitSyntax(..)
    , forgetSUnit
    , unitSing
    , unitVal
    , testEquivalentSUnit

    , Some(..)

      -- * Singletons for lists
    , SList(..)
    , KnownList(..)
    ) where

import Data.List (foldl')
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Type.Equality
import Data.Typeable
import Unsafe.Coerce

import Data.UnitsOfMeasure.Internal


class Typeable x => KnownBaseUnit (x :: BaseUnit) where
  baseUnitName :: String

-- | Singleton type for individual base units
data SBaseUnit (x :: BaseUnit) where
  SBaseUnit :: KnownBaseUnit x => SBaseUnit x

-- | Singleton type for concrete units of measure represented as lists
-- of base units
data SUnit (u :: UnitSyntax BaseUnit) where
  SUnit :: SList xs -> SList ys -> SUnit (xs :/ ys)

-- | Singleton type for lists of base units
data SList (xs :: [BaseUnit]) where
  SNil :: SList '[]
  SCons :: SBaseUnit x -> SList xs -> SList (x ': xs)

instance TestEquality SBaseUnit where
  testEquality (SBaseUnit @x) (SBaseUnit @y) = eqT @x @y

instance TestEquality SUnit where
  testEquality (SUnit xs ys) (SUnit xs' ys') = case (testEquality xs xs', testEquality ys ys') of
                                                 (Just Refl, Just Refl) -> Just Refl
                                                 _                      -> Nothing

instance TestEquality SList where
  testEquality SNil SNil = Just Refl
  testEquality (SCons x xs) (SCons y ys)
    | Just Refl <- testEquality x y
    , Just Refl <- testEquality xs ys = Just Refl
  testEquality _ _ = Nothing


-- | Test whether two 'SUnit's represent the same units, up to the
-- equivalence relation.
--
-- TODO: this currently uses 'unsafeCoerce', but in principle it should be
-- possible to avoid it.
--
-- This relies on sorting the units based on their fingerprint.  Technically we
-- can currently get away without assuming the 'Unpack' agrees with this, but we
-- might want it to, and might want to define:
--
-- normaliseSUnit :: SUnit u -> SUnit (Unpack (Pack u))
testEquivalentSUnit :: SUnit u -> SUnit v -> Maybe (Pack u :~: Pack v)
testEquivalentSUnit su sv
  | normaliseSUnit (forgetSUnit' su) == normaliseSUnit (forgetSUnit' sv) = Just (unsafeCoerce Refl)
  | otherwise = Nothing

normaliseSUnit :: UnitSyntax (Some SBaseUnit) -> Map.Map (Some SBaseUnit) Integer
normaliseSUnit (xs :/ ys) =
    Map.filter (/= 0)
        (foldl' (\ m x -> Map.insertWith (+) x (negate 1) m)
            (foldl' (\ m x -> Map.insertWith (+) x 1 m) Map.empty xs) ys)

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
-- fromList [("m",-1)]
--
-- >>> normaliseUnitSyntax ([] :/ ["m", "m"])
-- fromList [("m",-2)]
--
-- >>> normaliseUnitSyntax ([] :/ ["m", "m", "m"])
-- fromList [("m",-3)]
--
-- >>> normaliseUnitSyntax (["m"] :/ ["m"])
-- fromList []
--
-- >>> normaliseUnitSyntax (["m", "m"] :/ ["m"])
-- fromList [("m",1)]
--
-- >>> normaliseUnitSyntax (["m"] :/ ["m", "m"])
-- fromList [("m",-1)]
--
-- >>> normaliseUnitSyntax (["m", "m"] :/ ["m", "m"])
-- fromList []
--
-- >>> normaliseUnitSyntax (["m", "m", "m"] :/ ["m", "m"])
-- fromList [("m",1)]
--
-- >>> normaliseUnitSyntax (["m", "m"] :/ ["m", "m", "m"])
-- fromList [("m",-1)]
--
-- >>> normaliseUnitSyntax (["m", "m", "m"] :/ ["m", "m", "m"])
-- fromList []
--
-- >>> normaliseUnitSyntax (replicate 3 "m" :/ [])
-- fromList [("m",3)]
--
-- >>> normaliseUnitSyntax ([] :/ replicate 3 "m")
-- fromList [("m",-3)]
--
-- >>> normaliseUnitSyntax (replicate 3 "m" :/ replicate 3 "m")
-- fromList []
--
-- >>> normaliseUnitSyntax (["m"] :/ ["s"])
-- fromList [("m",1),("s",-1)]
--
-- >>> normaliseUnitSyntax (["m"] :/ ["s", "s"])
-- fromList [("m",1),("s",-2)]
--
-- >>> normaliseUnitSyntax (["m", "m"] :/ []) == normaliseUnitSyntax (["m", "m"] :/ ["m", "m"])
-- False
--
-- >>> normaliseUnitSyntax (["m"] :/ []) == normaliseUnitSyntax (["m"] :/ ["m", "m"])
-- False
normaliseUnitSyntax :: UnitSyntax String -> Map.Map String Integer
normaliseUnitSyntax (xs :/ ys) =
    Map.filter (/= 0)
        (foldl' (\ m x -> Map.insertWith (+) x (negate 1) m)
            (foldl' (\ m x -> Map.insertWith (+) x 1 m) Map.empty xs) ys)


-- | Extract the runtime syntactic representation from a singleton unit
forgetSUnit :: SUnit u -> UnitSyntax String
forgetSUnit (SUnit xs ys) = List.sort (forgetSList xs) :/ List.sort (forgetSList ys)

forgetSList :: SList xs -> [String]
forgetSList SNil = []
forgetSList (SCons (SBaseUnit @x) xs) = baseUnitName @x : forgetSList xs


-- | Extract the runtime syntactic representation from a singleton unit
forgetSUnit' :: SUnit u -> UnitSyntax (Some SBaseUnit)
forgetSUnit' (SUnit xs ys) = forgetSList' xs :/ forgetSList' ys

forgetSList' :: SList xs -> [Some SBaseUnit]
forgetSList' SNil = []
forgetSList' (SCons s xs) = Some s : forgetSList' xs


-- | A constraint @'KnownUnit' u@ means that @u@ must be a concrete
-- unit that is statically known but passed at runtime
type KnownUnit u = (u ~ Pack (Unpack u), KnownUnitSyntax (Unpack u))

class KnownUnitSyntax (u :: UnitSyntax BaseUnit) where
  unitSyntaxSing :: SUnit u

instance (KnownList xs, KnownList ys) => KnownUnitSyntax (xs :/ ys) where
  unitSyntaxSing = SUnit listSing listSing


-- | A constraint @'KnownList' xs@ means that @xs@ must be a list of
-- base units that is statically known but passed at runtime
class KnownList (xs :: [BaseUnit]) where
  listSing :: SList xs

instance KnownList '[] where
  listSing = SNil

instance (KnownBaseUnit x, KnownList xs) => KnownList (x ': xs) where
  listSing = SCons SBaseUnit listSing


unitSing :: forall u . KnownUnit u => SUnit (Unpack u)
unitSing = unitSyntaxSing @(Unpack u)

-- | Extract the runtime syntactic representation of a 'KnownUnit'
unitVal :: forall u . KnownUnit u => UnitSyntax String
unitVal = forgetSUnit (unitSing @u)



-- | An existential wrapper type: @'Some' p@ is essentially @exists x . p x@.
data Some p where
  Some :: p x -> Some p


instance Eq (Some SBaseUnit) where
  Some (SBaseUnit @x) == Some (SBaseUnit @y) =
      typeRepFingerprint (typeRep (Proxy @x)) == typeRepFingerprint (typeRep (Proxy @y))

instance Ord (Some SBaseUnit) where
  compare (Some (SBaseUnit @x)) (Some (SBaseUnit @y)) =
      compare (typeRepFingerprint (typeRep (Proxy @x)))
              (typeRepFingerprint (typeRep (Proxy @y)))
