{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

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
-- The above declarations can be written using the 'u' declaration
-- quasiquoter as @['u'| m, ft = 1 % 3.28 ft/m |]@, or generated
-- automatically using 'declareConvertibleUnit'.
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
--
-- If you wish to define a dimensionless unit that requires explicit
-- conversion to @1@, such as radians, write @['u'| rad = 1 1 |]@.
-- The syntax @['u'| dimensionless = 1 |]@ defines @dimensionless@ as
-- a unit synonym for @1@ that does not require conversion.
module Data.UnitsOfMeasure.Convert
    ( convert
    , ratio
    , HasCanonicalBaseUnit(..)
      -- * Constraints
    , Good
    , IsCanonical
    , HasCanonical
    , Convertible
    , ToCanonicalUnit
    ) where

import Data.UnitsOfMeasure.Internal
import Data.UnitsOfMeasure.Singleton

import Data.Kind (Type, Constraint)
import GHC.TypeLits (Symbol)


-- | Class to capture the dimensions to which base units belong.  For
-- a canonical base unit, the class instance can be left empty.
class IsCanonical (Unpack (CanonicalBaseUnit b))
    => HasCanonicalBaseUnit (b :: Symbol) where
  -- | The canonical base unit for this base unit.  If @b@ is
  -- canonical, then @'CanonicalBaseUnit' b = b@.  Otherwise,
  -- @'CanonicalBaseUnit' b@ must itself be canonical.
  type CanonicalBaseUnit b :: Unit
  type CanonicalBaseUnit b = Base b

  -- | The conversion ratio between this base unit and its canonical
  -- base unit.  If @b@ is canonical then this ratio is @1@.
  conversionBase :: proxy b -> Quantity Rational (Base b /: CanonicalBaseUnit b)
  default conversionBase :: (Base b ~ CanonicalBaseUnit b) => proxy b -> Quantity Rational (Base b /: CanonicalBaseUnit b)
  conversionBase _ = 1

-- | Convert a unit into its canonical representation, where units are
-- represented syntactically.
type ToCBU :: UnitSyntax Symbol -> Unit
type family ToCBU u where
  ToCBU (xs :/ ys) = ListToCBU xs /: ListToCBU ys

type ListToCBU :: [Symbol] -> Unit
type family ListToCBU xs where
  ListToCBU '[]       = One
  ListToCBU (x ': xs) = CanonicalBaseUnit x *: ListToCBU xs

-- | This constraint will be satisfied if all the base units in a
-- syntactically represented unit have associated canonical
-- representations.
type HasCanonical :: UnitSyntax Symbol -> Constraint
type family HasCanonical u where
  HasCanonical (xs :/ ys) = (AllHasCanonical xs, AllHasCanonical ys)

type AllHasCanonical :: [Symbol] -> Constraint
type family AllHasCanonical xs where
  AllHasCanonical '[] = ()
  AllHasCanonical (x ': xs) = (HasCanonicalBaseUnit x, AllHasCanonical xs)

-- | This constraint will be satisfied if all the base units in a
-- syntactically represented unit are in their canonical form.
type IsCanonical :: UnitSyntax Symbol -> Constraint
type family IsCanonical u where
  IsCanonical (xs :/ ys) = (AllIsCanonical xs, AllIsCanonical ys)

type AllIsCanonical :: [Symbol] -> Constraint
type family AllIsCanonical xs where
  AllIsCanonical '[] = ()
  AllIsCanonical (x ': xs) = (CanonicalBaseUnit x ~ Base x, AllIsCanonical xs)


conversionRatio :: forall proxy u . Good u
               => proxy u -> Quantity Rational (u /: ToCBU (Unpack u))
conversionRatio _ = help (unitSing :: SUnit (Unpack u))
{-# INLINABLE conversionRatio #-}

help :: forall u . HasCanonical u => SUnit u -> Quantity Rational (Pack u /: ToCBU u)
help (SUnit xs ys) = help' xs /: help' ys

help' :: forall xs . AllHasCanonical xs => SList xs -> Quantity Rational (Prod xs /: ListToCBU xs)
help' SNil         = 1
help' (SCons p xs) = conversionBase p *: help' xs


-- | A unit is "good" if all its base units have been defined, and
-- have associated canonical base units.
type Good            u = (u ~ Pack (Unpack u), KnownUnit (Unpack u), HasCanonical (Unpack u))

-- | Two units are convertible if they are both 'Good' and they have
-- the same canonical units (and hence the same dimension).
type Convertible   u v = (Good u, Good v, ToCanonicalUnit u ~ ToCanonicalUnit v)

-- | Converts a unit to the corresponding canonical representation.
type ToCanonicalUnit u = ToCBU (Unpack u)

-- | Automatically convert a quantity with units @u@ so that its units
-- are @v@, provided @u@ and @v@ have the same dimension.
convert :: forall a u v . (Fractional a, Convertible u v) => Quantity a u -> Quantity a v
convert = (ratio (undefined :: proxy' (proxy v)) (undefined :: proxy' (proxy u)) *:)
{-# INLINABLE convert #-}

-- | Calculate the conversion ratio between two units with the same
-- dimension.  The slightly unusual proxy arguments allow this to be
-- called using quasiquoters to specify the units, for example
-- @'ratio' [u| ft |] [u| m |]@.
ratio :: forall a u v (proxy :: Unit -> Type) proxy' .
         (Fractional a, Convertible u v)
      => proxy' (proxy u) -> proxy' (proxy v) -> Quantity a (u /: v)
ratio _ _ = fromRational' $ conversionRatio (undefined :: proxy u) /: conversionRatio (undefined :: proxy v)
{-# INLINABLE ratio #-}
