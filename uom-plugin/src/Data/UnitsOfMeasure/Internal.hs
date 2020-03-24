{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines the core types used in the @uom-plugin@
-- library.  Note that importing this module may allow you to violate
-- invariants, so you should generally work with the safe interface in
-- "Data.UnitsOfMeasure" instead.
module Data.UnitsOfMeasure.Internal
    ( -- * Type-level units of measure
      Unit
    , type One
    , type Base
    , type (*:)
    , type (/:)
    , type (^:)

      -- * Values indexed by their units
    , Quantity(..)
    , unQuantity
    , zero
    , mk

      -- * Unit-safe 'Num' operations
    , (+:)
    , (*:)
    , (-:)
    , negate'
    , abs'
    , signum'
    , fromInteger'

      -- * Unit-safe 'Fractional' operations
    , (/:)
    , recip'
    , fromRational'

      -- * Unit-safe 'Real' operations
    , toRational'

      -- * Unit-safe 'Floating' operations
    , sqrt'

      -- * Syntactic representation of units
    , UnitSyntax(..)
    , Unpack
    , Pack
    , Prod

      -- * Internal
    , type (~~)
    , MkUnit
    ) where

import Control.DeepSeq
import Foreign.Storable
import GHC.Exts (Constraint)
import GHC.TypeLits (Symbol, Nat, type (-))

-- | (Kind) Units of measure
data Unit

-- | Dimensionless unit (identity element)
type family One :: Unit
#if __GLASGOW_HASKELL__ >= 711
  where
#endif

-- | Base unit
type family Base (b :: Symbol) :: Unit
#if __GLASGOW_HASKELL__ >= 711
  where
#endif

-- | Multiplication for units of measure
type family (u :: Unit) *: (v :: Unit) :: Unit
#if __GLASGOW_HASKELL__ >= 711
  where
#endif

-- | Division for units of measure
type family (u :: Unit) /: (v :: Unit) :: Unit
#if __GLASGOW_HASKELL__ >= 711
  where
#endif

-- | Exponentiation (to a positive power) for units of measure;
-- negative exponents are not yet supported (they require an Integer kind)
type family (u :: Unit) ^: (n :: Nat)  :: Unit where
  u ^: 0 = One
  u ^: 1 = u
  u ^: n = u *: (u ^: (n-1))

infixl 6 +:, -:
infixl 7 *:, /:
infixr 8 ^:

-- | A @Quantity a u@ is represented identically to a value of
-- underlying numeric type @a@, but with units @u@.
newtype Quantity a (u :: Unit) = MkQuantity a
  -- ^ Warning: the 'MkQuantity' constructor allows module invariants
  -- to be violated, so use it with caution!
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

-- To enable marshalling into FFI code.
deriving instance Storable a => Storable (Quantity a u)

-- To enable deriving NFData for data types containing Quantity fields.
deriving instance NFData a => NFData (Quantity a u)

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


-- | Addition ('+') of quantities requires the units to match.
(+:) :: Num a => Quantity a u -> Quantity a u -> Quantity a u
MkQuantity x +: MkQuantity y = MkQuantity (x + y)

-- | Multiplication ('*') of quantities multiplies the units.
(*:) :: (Num a, w ~~ u *: v) => Quantity a u -> Quantity a v -> Quantity a w
MkQuantity x *: MkQuantity y = MkQuantity (x * y)

-- | Subtraction ('-') of quantities requires the units to match.
(-:) :: Num a => Quantity a u -> Quantity a u -> Quantity a u
MkQuantity x -: MkQuantity y = MkQuantity (x - y)

-- | Negation ('negate') of quantities is polymorphic in the units.
negate' :: Num a => Quantity a u -> Quantity a u
negate' (MkQuantity x) = MkQuantity (negate x)

-- | Absolute value ('abs') of quantities is polymorphic in the units.
abs' :: Num a => Quantity a u -> Quantity a u
abs' (MkQuantity x) = MkQuantity (abs x)

-- | The sign ('signum') of a quantity gives a dimensionless result.
signum' :: Num a => Quantity a u -> Quantity a One
signum' (MkQuantity x) = MkQuantity (signum x)

-- | Convert an 'Integer' quantity into any 'Integral' type ('fromInteger').
fromInteger' :: Integral a => Quantity Integer u -> Quantity a u
fromInteger' (MkQuantity x) = MkQuantity (fromInteger x)


-- | Division ('/') of quantities divides the units.
(/:) :: (Fractional a, w ~~ u /: v) => Quantity a u -> Quantity a v -> Quantity a w
MkQuantity x /: MkQuantity y = MkQuantity (x / y)

-- | Reciprocal ('recip') of quantities reciprocates the units.
recip' :: (Fractional a, w ~~ One /: u) => Quantity a u -> Quantity a w
recip' (MkQuantity x) = MkQuantity (recip x)

-- | Convert a 'Rational' quantity into any 'Fractional' type ('fromRational').
fromRational' :: Fractional a => Quantity Rational u -> Quantity a u
fromRational' (MkQuantity x) = MkQuantity (fromRational x)

-- | Convert any 'Real' quantity into a 'Rational' type ('toRational').
toRational' :: Real a => Quantity a u -> Quantity Rational u
toRational' (MkQuantity x) = MkQuantity (toRational x)


-- | Taking the square root ('sqrt') of a quantity requires its units
-- to be a square.  Fractional units are not currently supported.
sqrt' :: (Floating a, w ~~ u ^: 2) => Quantity a w -> Quantity a u
sqrt' (MkQuantity x) = MkQuantity (sqrt x)


-- | Syntactic representation of a unit as a pair of lists of base
-- units, for example 'One' is represented as @[] ':/' []@ and
-- @'Base' "m" '/:' 'Base' "s" ^: 2@ is represented as @["m"] ':/' ["s","s"]@.
data UnitSyntax s = [s] :/ [s]
  deriving (Eq, Show)

-- | Pack up a syntactic representation of a unit as a unit.  For example:
--
-- @ 'Pack' ([] ':/' []) = 'One' @
--
-- @ 'Pack' (["m"] ':/' ["s","s"]) = 'Base' "m" '/:' 'Base' "s" ^: 2 @
--
-- This is a perfectly ordinary closed type family.  'Pack' is a left
-- inverse of 'Unpack' up to the equational theory of units, but it is
-- not a right inverse (because there are multiple list
-- representations of the same unit).
type family Pack (u :: UnitSyntax Symbol) :: Unit where
  Pack (xs :/ ys) = Prod xs /: Prod ys

-- | Take the product of a list of base units.
type family Prod (xs :: [Symbol]) :: Unit where
  Prod '[]       = One
  Prod (x ': xs) = Base x *: Prod xs

-- | Unpack a unit as a syntactic representation, where the order of
-- units is deterministic.  For example:
--
-- @ 'Unpack' 'One' = [] ':/' [] @
--
-- @ 'Unpack' ('Base' "s" '*:' 'Base' "m") = ["m","s"] ':/' [] @
--
-- This does not break type soundness because
-- 'Unpack' will reduce only when the unit is entirely constant, and
-- it does not allow the structure of the unit to be observed.  The
-- reduction behaviour is implemented by the plugin, because we cannot
-- define it otherwise.
type family Unpack (u :: Unit) :: UnitSyntax Symbol
#if __GLASGOW_HASKELL__ >= 711
  where
#endif


-- | This is a bit of a hack, honestly, but a good hack.  Constraints
-- @u ~~ v@ are just like equalities @u ~ v@, except solving them will
-- be delayed until the plugin.  This may lead to better inferred types.
type family (u :: Unit) ~~ (v :: Unit) :: Constraint
#if __GLASGOW_HASKELL__ >= 711
  where
#endif

infix 4 ~~


-- | This type family is used for translating unit names (as
-- type-level strings) into units.  It will be 'Base' for base units
-- or expand the definition for derived units.
--
-- The instances displayed by Haddock are available only if
-- "Data.UnitsOfMeasure.Defs" is imported.
type family MkUnit (s :: Symbol) :: Unit
