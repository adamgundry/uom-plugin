{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.UnitsOfMeasure.Internal
    ( Unit(Base)
    , type One
    , type (*:)
    , type (/:)
    , type (^:)
    , Quantity(..)
    ) where

import GHC.TypeLits (Symbol, Nat)

-- | (Kind) Units of measure
data Unit = Base Symbol

-- | Dimensionless unit (identity element)
type family One :: Unit

-- | Multiplication for units of measure
type family (u :: Unit) *: (v :: Unit) :: Unit

-- | Division for units of measure
type family (u :: Unit) /: (v :: Unit) :: Unit

-- | Exponentiation (to a positive power) for units of measure;
-- negative exponents are not yet supported (they require an Integer kind)
type family (u :: Unit) ^: (n :: Nat)  :: Unit

infixl 7 *:, /:
infixr 8 ^:

-- | A @Quantity a u@ is represented identically to a value of
-- underlying numeric type @a@, but with units @u@.
newtype Quantity a (u :: Unit) = MkQuantity a
type role Quantity representational nominal

-- These classes work uniformly on the underlying representation,
-- regardless of the units
deriving instance Bounded a => Bounded (Quantity a u)
deriving instance Eq      a => Eq      (Quantity a u)
deriving instance Ord     a => Ord     (Quantity a u)
deriving instance Show    a => Show    (Quantity a u)

-- These classes are not unit-polymorphic, so we have to restrict the
-- unit index to be dimensionless
deriving instance (Enum       a, u ~ One) => Enum       (Quantity a u)
deriving instance (Floating   a, u ~ One) => Floating   (Quantity a u)
deriving instance (Fractional a, u ~ One) => Fractional (Quantity a u)
deriving instance (Integral   a, u ~ One) => Integral   (Quantity a u)
deriving instance (Num        a, u ~ One) => Num        (Quantity a u)
deriving instance (Real       a, u ~ One) => Real       (Quantity a u)
deriving instance (RealFloat  a, u ~ One) => RealFloat  (Quantity a u)
deriving instance (RealFrac   a, u ~ One) => RealFrac   (Quantity a u)
