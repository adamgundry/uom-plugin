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

module Data.UnitsOfMeasure
    ( Unit(..)
    , type (*:)
    , type (/:)
    , type (^:)
    , Quantity(unQuantity) -- N.B. MkQuantity not exported!
    , zero
    , mk
    , (%)
    , (+:)
    , (-:)
    , (*:)
    , (/:)
    , negate'
    , sqrt'
    , MkUnit
    ) where

import GHC.Prim (Proxy#, proxy#)
import GHC.TypeLits

-- | (Kind) Units of measure
data Unit = One | Base Symbol

-- | Multiplication for units of measure
type family (u :: Unit) *: (v :: Unit) :: Unit

-- | Division for units of measure
type family (u :: Unit) /: (v :: Unit) :: Unit

-- | Exponentiation (to a positive power) for units of measure;
-- negative exponents are not yet supported (they require an Integer kind)
type family (u :: Unit) ^: (n :: Nat)  :: Unit

infixl 7 *:, /:
infixr 8 ^:


type role Quantity representational nominal
newtype Quantity a (u :: Unit) = MkQuantity { unQuantity :: a }
  deriving (Bounded, Enum, Eq, Ord, Show)

zero :: Num a => Quantity a u
zero = MkQuantity 0

mk :: a -> Quantity a One
mk = MkQuantity

-- | Assign a unit to a quantity.
-- This must always be applied to a concrete second argument
-- (generalizing over @u@ allows the unit system to be broken)!
(%) :: a -> Proxy# u -> Quantity a u
x % _ = MkQuantity x

(+:) :: Num a => Quantity a u -> Quantity a u -> Quantity a u
MkQuantity x +: MkQuantity y = MkQuantity (x + y)

(-:) :: Num a => Quantity a u -> Quantity a u -> Quantity a u
MkQuantity x -: MkQuantity y = MkQuantity (x - y)

(*:) :: Num a => Quantity a u -> Quantity a v -> Quantity a (u *: v)
MkQuantity x *: MkQuantity y = MkQuantity (x * y)

(/:) :: Fractional a => Quantity a u -> Quantity a v -> Quantity a (u /: v)
MkQuantity x /: MkQuantity y = MkQuantity (x / y)

infixl 6 +:, -:

sqrt' :: Floating a => Quantity a (u *: u) -> Quantity a u
sqrt' (MkQuantity x) = MkQuantity (sqrt x)

negate' :: Num a => Quantity a u -> Quantity a u
negate' (MkQuantity x) = MkQuantity (negate x)


deriving instance (Floating   a, u ~ One) => Floating   (Quantity a u)
deriving instance (Fractional a, u ~ One) => Fractional (Quantity a u)
deriving instance (Integral   a, u ~ One) => Integral   (Quantity a u)
deriving instance (Num        a, u ~ One) => Num        (Quantity a u)
deriving instance (Real       a, u ~ One) => Real       (Quantity a u)
deriving instance (RealFloat  a, u ~ One) => RealFloat  (Quantity a u)
deriving instance (RealFrac   a, u ~ One) => RealFrac   (Quantity a u)


type family MkUnit (s :: Symbol) :: Unit
