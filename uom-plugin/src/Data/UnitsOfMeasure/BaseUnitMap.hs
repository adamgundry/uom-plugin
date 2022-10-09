{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.UnitsOfMeasure.BaseUnitMap
  ( BaseUnitMap
  , lookupBaseUnitMap
  , mkBaseUnitMap
  , singletonBaseUnitMap
  , unitToBaseUnitMap
  , Some(..)
  ) where

import qualified Data.Map.Strict as Map

import Data.UnitsOfMeasure.Internal
import Data.UnitsOfMeasure.Singleton

-- | An existential wrapper type: @'Some' p@ is essentially @exists x . p x@.
data Some p where
  Some :: p x -> Some p


-- | A map from base unit names to the corresponding types.
newtype BaseUnitMap = MkBaseUnitMap (Map.Map String (Some SBaseUnit))
  deriving (Semigroup, Monoid)

lookupBaseUnitMap :: String -> BaseUnitMap -> Maybe (Some SBaseUnit)
lookupBaseUnitMap s (MkBaseUnitMap m) = Map.lookup s m

mkBaseUnitMap :: [Some SBaseUnit] -> BaseUnitMap
mkBaseUnitMap = MkBaseUnitMap . Map.fromList . map (\ s@(Some (SBaseUnit :: SBaseUnit x)) -> (baseUnitName @x, s))

singletonBaseUnitMap :: forall x. KnownBaseUnit x => BaseUnitMap
singletonBaseUnitMap = mkBaseUnitMap [Some (SBaseUnit @x)]

unitToBaseUnitMap :: SUnit u -> BaseUnitMap
unitToBaseUnitMap (SUnit xs ys) = mkBaseUnitMap (listToBaseUnits xs ++ listToBaseUnits ys)

listToBaseUnits :: SList xs -> [Some SBaseUnit]
listToBaseUnits SNil = []
listToBaseUnits (SCons x xs) = Some x : listToBaseUnits xs
