{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.UnitsOfMeasure.Internal
    ( Unit
    , type One
    , type Base
    , type (*:)
    , type (/:)
    , type (^:)
    , Quantity(..)

    , TypeInt(..)
    , Unpack

    , type (~~)

    , MkUnit
    ) where

import GHC.Exts (Constraint)
import GHC.TypeLits (Symbol, Nat)

-- | (Kind) Units of measure
data Unit

-- | Dimensionless unit (identity element)
type family One :: Unit

-- | Base unit
type family Base (b :: Symbol) :: Unit

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


-- | Type-level integers, represented as wrapped type-level naturals
-- with redundant zeros (but we won't ever use @'Neg' 0@).
data TypeInt = Pos Nat | Neg Nat

-- | Unpack a unit as a list of (base unit, exponent) pairs, where the
-- order is deterministic and the exponent is never zero.  This does
-- not break type soundness because 'Unpack' will reduce only when the
-- unit is entirely constant, and it does not allow the structure of
-- the unit to be observed.  The reduction behaviour is implemented by
-- the plugin, because we cannot define it otherwise.
type family Unpack (u :: Unit) :: [(Symbol, TypeInt)]


-- | This is a bit of a hack, honestly, but a good hack.  Constraints
-- @u ~~ v@ are just like equalities @u ~ v@, except solving them will
-- be delayed until the plugin.  This may lead to better inferred types.
type family (u :: Unit) ~~ (v :: Unit) :: Constraint

infix 4 ~~


-- | This type family is used for translating unit names (as
-- type-level strings) into units.  It will be 'Base' for base units
-- or expand the definition for derived units.
type family MkUnit (s :: Symbol) :: Unit
