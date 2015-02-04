{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.UnitsOfMeasure
    ( -- * Type-level units of measure
      Unit
    , type Base
    , type One
    , type (*:)
    , type (/:)
    , type (^:)

      -- * Values indexed by their units
    , Quantity
    , unQuantity
    , zero
    , mk

      -- * Unit-safe arithmetic operations
    , (+:)
    , (-:)
    , (*:)
    , (/:)
    , sqrt'
    , negate'
    , recip'

      -- * Pay no attention to that man behind the curtain
    , MkUnit

    , TypeInt(..)
    , type (^^:)
    , Pack
    , Unpack
    ) where

import GHC.TypeLits

import Data.UnitsOfMeasure.Internal


-- | Extract the underlying value of a quantity
unQuantity :: Quantity a u -> a
unQuantity (MkQuantity x) = x

-- | Zero is polymorphic in its units: this is required because the
-- 'Num' instance constrains the quantity to be dimensionless, so
-- @0 :: Quantity a u@ is not well typed.
zero :: Num a => Quantity a u
zero = MkQuantity 0

-- | Construct a 'Quantity' from a dimensionless value.  Note that for
-- numeric literals, the 'Num' and 'Fractional' instances allow them
-- to be treated as quantities directly.
mk :: a -> Quantity a One
mk = MkQuantity

-- | Addition of quantities requires the units to match.
(+:) :: Num a => Quantity a u -> Quantity a u -> Quantity a u
MkQuantity x +: MkQuantity y = MkQuantity (x + y)

-- | Subtraction of quantities requires the units to match.
(-:) :: Num a => Quantity a u -> Quantity a u -> Quantity a u
MkQuantity x -: MkQuantity y = MkQuantity (x - y)

-- | Multiplication of quantities multiplies the units.
(*:) :: Num a => Quantity a u -> Quantity a v -> Quantity a (u *: v)
MkQuantity x *: MkQuantity y = MkQuantity (x * y)

-- | Division of quantities divides the units.
(/:) :: Fractional a => Quantity a u -> Quantity a v -> Quantity a (u /: v)
MkQuantity x /: MkQuantity y = MkQuantity (x / y)

infixl 6 +:, -:
infixl 7 *:, /:

-- | Taking the square root of a quantity requires its units to be a
-- square.  Fractional units are not currently supported.  This
-- operation is provided as a primitive because it is not otherwise
-- definable.
sqrt' :: Floating a => Quantity a (u ^: 2) -> Quantity a u
sqrt' (MkQuantity x) = MkQuantity (sqrt x)

-- | Negation of quantities is polymorphic in the units.
negate' :: Num a => Quantity a u -> Quantity a u
negate' (MkQuantity x) = MkQuantity (negate x)

-- | Reciprocal of quantities reciprocates the units.
recip' :: Fractional a => Quantity a u -> Quantity a (One /: u)
recip' (MkQuantity x) = MkQuantity (recip x)


-- | This type family is used for translating unit names (as
-- type-level strings) into units.  It will be 'Base' for base units
-- or expand the definition for derived units.
type family MkUnit (s :: Symbol) :: Unit


-- | Unit exponentiation for type-level integers
type family (^^:) (u :: Unit) (i :: TypeInt) :: Unit where
  u ^^: Pos n = u ^: n
  u ^^: Neg n = One /: (u ^: n)

-- | Pack a list of (base unit, exponent) pairs as a unit.  This is a
-- perfectly ordinary closed type family.  'Pack' is a left inverse of
-- 'Unpack' up to the equational theory of units, but it is not a
-- right inverse (because there are multiple list representations of
-- the same unit).
type family Pack (xs :: [(Symbol, TypeInt)]) :: Unit where
  Pack '[]             = One
  Pack ('(b, i) ': xs) = (Base b ^^: i) *: Pack xs
