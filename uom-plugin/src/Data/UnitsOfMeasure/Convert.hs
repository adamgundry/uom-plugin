{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -ddump-tc-trace -ddump-to-file #-}

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
-- For example, to declare @U_m@ as a canonical base unit, write:
--
-- > instance HasCanonicalBaseUnit U_m
--
-- To declare @ft@ as a derived unit, write:
--
-- > instance HasCanonicalBaseUnit U_ft where
-- >   type CanonicalBaseUnit U_ft = U_m
-- >   conversionBase = [u| 3.28 ft/m |]
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
    , integralConvert
    , integralRatio
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

import Data.Kind (Constraint, Type)


-- | Class to capture the dimensions to which base units belong.  For
-- a canonical base unit, the class instance can be left empty.
class IsCanonical (Unpack (CanonicalBaseUnit b))
    => HasCanonicalBaseUnit (b :: BaseUnit) where
  -- | The canonical base unit for this base unit.  If @b@ is
  -- canonical, then @'CanonicalBaseUnit' b = b@.  Otherwise,
  -- @'CanonicalBaseUnit' b@ must itself be canonical.
  type CanonicalBaseUnit b :: Unit
  type CanonicalBaseUnit b = b

  type ConversionRatioConstraints b a :: Constraint
  type ConversionRatioConstraints b a = Fractional a

  -- | The conversion ratio between this base unit and its canonical
  -- base unit.  If @b@ is canonical then this ratio is @1@.
  conversionBase :: ConversionRatioConstraints b a => Quantity a (CanonicalBaseUnit b /: b)
  default conversionBase :: (Num a, b ~ CanonicalBaseUnit b) => Quantity a (CanonicalBaseUnit b /: b)
  conversionBase = 1

-- | Convert a unit into its canonical representation, where units are
-- represented syntactically.
type ToCBU :: UnitSyntax BaseUnit -> Unit
type family ToCBU u where
  ToCBU (xs :/ ys) = ListToCBU xs /: ListToCBU ys

type ListToCBU :: [BaseUnit] -> Unit
type family ListToCBU xs where
  ListToCBU '[]       = One
  ListToCBU (x ': xs) = CanonicalBaseUnit x *: ListToCBU xs

-- | This constraint will be satisfied if all the base units in a
-- syntactically represented unit have associated canonical
-- representations.
type HasCanonical :: Type -> UnitSyntax BaseUnit -> Constraint
type family HasCanonical r u where
  HasCanonical r (xs :/ ys) = (AllHasCanonical r xs, AllHasCanonical r ys)

type AllHasCanonical :: Type -> [BaseUnit] -> Constraint
type family AllHasCanonical r xs where
  AllHasCanonical _ '[] = ()
  AllHasCanonical r (x ': xs) = (HasCanonicalBaseUnit x, AllHasCanonical r xs, ConversionRatioConstraints x r)

-- | This constraint will be satisfied if all the base units in a
-- syntactically represented unit are in their canonical form.
type IsCanonical :: UnitSyntax BaseUnit -> Constraint
type family IsCanonical u where
  IsCanonical (xs :/ ys) = (AllIsCanonical xs, AllIsCanonical ys)

type AllIsCanonical :: [BaseUnit] -> Constraint
type family AllIsCanonical xs where
  AllIsCanonical '[] = ()
  AllIsCanonical (x ': xs) = (CanonicalBaseUnit x ~ x, AllIsCanonical xs)


conversionRatio :: forall u a . (Good a u, Fractional a) => Quantity a (ToCanonicalUnit u /: u)
conversionRatio = help (unitSing @u)
{-# INLINABLE conversionRatio #-}

help :: forall a u . (HasCanonical a u, Fractional a) => SUnit u -> Quantity a (ToCBU u /: Pack u)
help (SUnit xs ys) = help' xs /: help' ys

help' :: forall a xs . (AllHasCanonical a xs, Num a) => SList xs -> Quantity a (ListToCBU xs /: Prod xs)
help' SNil = 1
help' (SCons (_ :: SBaseUnit x) xs) = conversionBase @x *: help' xs

help2 :: forall a u . (NoInverseUnits u, HasCanonical a u, Num a) => SUnit u -> Quantity a (ToCBU u /: Pack u)
help2 (SUnit xs SNil) = help' xs

type family NoInverseUnits (u :: UnitSyntax b) :: Constraint where
  NoInverseUnits (_ :/ ys) = (ys ~ '[])



-- | A unit is "good" if all its base units have been defined, and
-- have associated canonical base units.
type Good r u = (KnownUnit u, HasCanonical r (Unpack u))

-- | Two units are convertible if they are both 'Good' and they have
-- the same canonical units (and hence the same dimension).
type Convertible r u v = (Good r u, Good r v, ToCanonicalUnit u ~ ToCanonicalUnit v)

-- | Converts a unit to the corresponding canonical representation.
type ToCanonicalUnit u = ToCBU (Unpack u)

-- | Automatically convert a quantity with units @u@ so that its units
-- are @v@, provided @u@ and @v@ have the same dimension.
convert :: forall a u v . (Convertible a u v, Fractional a) => Quantity a u -> Quantity a v
convert = (ratio @a @v @u *:)
{-# INLINABLE convert #-}

-- | Calculate the conversion ratio between two units with the same dimension.
-- This should be called using type applications to specify the units, for
-- example @'ratio' @[u| ft |] @[u| m |]@.
ratio :: forall a u v . (Convertible a u v, Fractional a) => Quantity a (u /: v)
ratio = conversionRatio @v /: conversionRatio @u
{-# INLINABLE ratio #-}


-- | Automatically convert a quantity with units @u@ so that its units are @v@,
-- provided @v@ is the canonical base unit for @u@ and the conversion ratios are
-- all integral.  Note that @v@ is uniquely determined from @u@ so it is not
-- necessary to annotate the result type.
--
-- >>> integralConvert @Integer [u| 3 km |]
-- [u| 3000 m |]
--
integralConvert :: forall a u v . (Convertible a u v, Num a, NoInverseUnits (Unpack u), v ~ ToCanonicalUnit v) => Quantity a u -> Quantity a v
integralConvert = (integralRatio @a @u @v *:)
{-# INLINABLE integralConvert #-}

-- | Calculate the conversion ratio between two units with the same dimension.
-- This should be called using type applications to specify the units, for
-- example @'ratio' @[u| ft |] @[u| m |]@.
integralRatio :: forall a u v . (Convertible a u v, Num a, NoInverseUnits (Unpack u), v ~ ToCanonicalUnit v) => Quantity a (v /: u)
integralRatio = help2 @a @(Unpack u) (unitSing @u)
{-# INLINABLE integralRatio #-}
