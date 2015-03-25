{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

-- | Experimental support for conversions between units with the same
-- dimension, for example feet and metres.
module Data.UnitsOfMeasure.Convert
    ( convert
    , ratio
    , HasCanonicalBaseUnit(..)
    ) where

import Data.UnitsOfMeasure
import Data.UnitsOfMeasure.Internal
import Data.UnitsOfMeasure.Singleton

import GHC.Exts ( Constraint )
import GHC.TypeLits


-- | Class to capture the dimensions to which base units belong.
-- Rather than defining dimensions explicitly, we pick a "canonical"
-- base unit for each dimension, and record the conversion ratio
-- between each base unit and the canonical base unit for its
-- dimension.  For a canonical base unit, the class instance can be
-- left empty.
class HasCanonicalBaseUnit (b :: Symbol) where
  -- | The canonical base unit for this base unit.  If @b@ is
  -- canonical, then @'CanonicalBaseUnit' b = b@.
  type CanonicalBaseUnit b :: Symbol
  type CanonicalBaseUnit b = b

  -- | The conversion ratio between this base unit and its canonical
  -- base unit.  If @b@ is canonical then this ratio is @1@.
  conversionBase :: Fractional a => proxy b -> Quantity a (Base b /: Base (CanonicalBaseUnit b))
  default conversionBase :: (Fractional a, b ~ CanonicalBaseUnit b) => proxy b -> Quantity a (Base b /: Base b)
  conversionBase _ = 1

type family MapCBU (xs :: [(Symbol, TypeInt)]) :: [(Symbol, TypeInt)] where
  MapCBU '[]             = '[]
  MapCBU ('(b, i) ': xs) = '(CanonicalBaseUnit b, i) ': MapCBU xs

type family HasCanonical (xs :: [(Symbol, TypeInt)]) :: Constraint where
  HasCanonical '[]             = ()
  HasCanonical ('(b, i) ': xs) = (HasCanonicalBaseUnit b, HasCanonical xs)


conversionRatio :: forall proxy a u . ( Fractional a
                                      , KnownUnit (Unpack u)
                                      , HasCanonical (Unpack u)
                                      , u ~ Pack (Unpack u)
                                      )
               => proxy u -> Quantity a (u /: Pack (MapCBU (Unpack u)))
conversionRatio _ = help (unitSing :: SUnit (Unpack u))

help :: forall a xs . (Fractional a, HasCanonical xs) => SUnit xs -> Quantity a (Pack xs /: Pack (MapCBU xs))
help SNil          = 1
help (SCons p i x) = unsafeConvertQuantity $ power (conversionBase p) i *: help x


-- | TODO: why does 'help' still need this? It fails to deduce this:
--     (((('Base b ^^: i) *: Pack xs1) /: (('Base (CanonicalBaseUnit b) ^^: i) *: Pack (MapCBU xs1)))
--   ~ ((('Base b /: 'Base (CanonicalBaseUnit b)) ^^: i) *: (Pack xs1 /: Pack (MapCBU xs1))))
unsafeConvertQuantity :: Quantity a u -> Quantity a v
unsafeConvertQuantity = MkQuantity . unQuantity


type Good u = (u ~ Pack (Unpack u), KnownUnit (Unpack u), HasCanonical (Unpack u))
type SameDimension u v = Pack (MapCBU (Unpack u)) ~ Pack (MapCBU (Unpack v))

-- | Automatically convert a quantity with units @u@ so that its units
-- are @v@, provided @u@ and @v@ have the same dimension.
convert :: forall a u v . (Fractional a, Good u, Good v, SameDimension u v)
         => Quantity a u -> Quantity a v
convert = (r *:)
  where
    r = conversionRatio (undefined :: proxy v) /: conversionRatio (undefined :: proxy u)

-- | Calculate the conversion ratio between two units with the same
-- dimension.  The slightly unusual proxy arguments allow this to be
-- called using quasiquoters to specify the units, for example
-- @'ratio' [u| ft |] [u| m |]@.
ratio :: forall a u v (proxy :: Unit -> *) proxy' .
         (Fractional a, Good u, Good v, SameDimension u v)
      => proxy' (proxy u) -> proxy' (proxy v) -> Quantity a (u /: v)
ratio _ _ = conversionRatio (undefined :: proxy u) /: conversionRatio (undefined :: proxy v)
