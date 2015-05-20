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
-- dimension, for example feet and metres.  This interface is not
-- necessarily stable!
--
-- Rather than defining dimensions explicitly, we pick a "canonical"
-- base unit for each dimension, and record the conversion ratio
-- between each base unit and the canonical base unit for its
-- dimension.  This means we can automatically calculate the
-- conversion ratio between a unit and its canonical representation,
-- and hence between any two units that share a dimension (i.e. have
-- the same canonical representation).
--
-- For example, to declare @m@ as a canonical base unit, write:
--
-- > instance HasCanonicalBaseUnit "m"
--
-- To declare @ft@ as a derived unit, write:
--
-- > instance HasCanonicalBaseUnit "ft" where
-- >   type CanonicalBaseUnit "ft" = "m"
-- >   conversionBase _ = [u| 3.28 ft/m |]
--
-- Now it is possible to 'convert' between quantities whose units
-- involve feet or metres.  For example:
--
-- >>> convert [u| 10m |] :: Quantity Double [u| ft |]
-- [u| 32.8 ft |]
-- >>> convert [u| 3ft^2 |] :: Quantity Double [u| m^2 |]
-- [u| 0.27885187388459254 m^2 |]
--
-- You are likely to get unpleasant compiler error messages if you
-- attempt to convert without the units being fully determined by type
-- inference, or if the units do not have the same dimension.
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


-- | Class to capture the dimensions to which base units belong.  For
-- a canonical base unit, the class instance can be left empty.
class (CanonicalBaseUnit (CanonicalBaseUnit b) ~ CanonicalBaseUnit b)
    => HasCanonicalBaseUnit (b :: Symbol) where
  -- | The canonical base unit for this base unit.  If @b@ is
  -- canonical, then @'CanonicalBaseUnit' b = b@.  Otherwise,
  -- @'CanonicalBaseUnit' b@ must itself be canonical.
  type CanonicalBaseUnit b :: Symbol
  type CanonicalBaseUnit b = b

  -- | The conversion ratio between this base unit and its canonical
  -- base unit.  If @b@ is canonical then this ratio is @1@.
  conversionBase :: proxy b -> Quantity Rational (Base b /: Base (CanonicalBaseUnit b))
  default conversionBase :: (b ~ CanonicalBaseUnit b) => proxy b -> Quantity Rational (Base b /: Base b)
  conversionBase _ = 1

type family MapCBU (xs :: [(Symbol, TypeInt)]) :: [(Symbol, TypeInt)] where
  MapCBU '[]             = '[]
  MapCBU ('(b, i) ': xs) = '(CanonicalBaseUnit b, i) ': MapCBU xs

type family HasCanonical (xs :: [(Symbol, TypeInt)]) :: Constraint where
  HasCanonical '[]             = ()
  HasCanonical ('(b, i) ': xs) = (HasCanonicalBaseUnit b, HasCanonical xs)


conversionRatio :: forall proxy u . Good u
               => proxy u -> Quantity Rational (u /: Pack (MapCBU (Unpack u)))
conversionRatio _ = help (unitSing :: SUnit (Unpack u))

help :: forall xs . HasCanonical xs => SUnit xs -> Quantity Rational (Pack xs /: Pack (MapCBU xs))
help SNil          = 1
help (SCons p i x) = unsafeConvertQuantity $ power (conversionBase p) i *: help x

power :: Quantity Rational u -> STypeInt i -> Quantity Rational (u ^^: i)
power (MkQuantity x) (SPos p) = MkQuantity (x ^^ natVal p)
power (MkQuantity x) (SNeg p) = MkQuantity (x ^^ (- natVal p))


-- | TODO: why does 'help' still need this? It fails to deduce this:
--     (((('Base b ^^: i) *: Pack xs1) /: (('Base (CanonicalBaseUnit b) ^^: i) *: Pack (MapCBU xs1)))
--   ~ ((('Base b /: 'Base (CanonicalBaseUnit b)) ^^: i) *: (Pack xs1 /: Pack (MapCBU xs1))))
unsafeConvertQuantity :: Quantity a u -> Quantity a v
unsafeConvertQuantity = MkQuantity . unQuantity


type Good            u = (u ~ Pack (Unpack u), KnownUnit (Unpack u), HasCanonical (Unpack u))
type Convertible   u v = (Good u, Good v, ToCanonicalUnit u ~ ToCanonicalUnit v)
type ToCanonicalUnit u = Pack (MapCBU (Unpack u))

-- | Automatically convert a quantity with units @u@ so that its units
-- are @v@, provided @u@ and @v@ have the same dimension.
convert :: forall a u v . (Fractional a, Convertible u v) => Quantity a u -> Quantity a v
convert = (ratio (undefined :: proxy' (proxy v)) (undefined :: proxy' (proxy u)) *:)

-- | Calculate the conversion ratio between two units with the same
-- dimension.  The slightly unusual proxy arguments allow this to be
-- called using quasiquoters to specify the units, for example
-- @'ratio' [u| ft |] [u| m |]@.
ratio :: forall a u v (proxy :: Unit -> *) proxy' .
         (Fractional a, Convertible u v)
      => proxy' (proxy u) -> proxy' (proxy v) -> Quantity a (u /: v)
ratio _ _ = fromRational' $ conversionRatio (undefined :: proxy u) /: conversionRatio (undefined :: proxy v)
