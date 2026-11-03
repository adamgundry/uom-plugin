{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

-- A module deliberately compiled without the plugin, to ensure that code
-- with only trivial units typechecks without the plugin.
module NoPlugin
  ( Quantities
  , square
  , squareDouble
  , twice
  , twiceDouble
  ) where

import Prelude hiding ((*))
import qualified Prelude
import Data.Kind (Constraint, Type)

import Data.UnitsOfMeasure.Internal (Quantity(MkQuantity), Unit(One), type (*:))

-- Multiplication graded by units: the product of values of degrees @u@ and
-- @v@ has degree @u *: v@.
class Graded g where
  type Homogeneous g (u :: Unit) = (r :: Type) | r -> g u
  type Support g :: Unit -> Constraint
  (*) :: (Support g u, Support g v)
      => Homogeneous g u -> Homogeneous g v -> Homogeneous g (u *: v)

class    u ~ One => IsOne u
instance u ~ One => IsOne u

class    AnyDegree (u :: Unit)
instance AnyDegree u

-- Without units: the only degree is 'One'.
instance Graded Double where
  type Homogeneous Double One = Double
  type Support Double = IsOne
  (*) = (Prelude.*)

-- With units: the component of degree @u@ is @'Quantity' a u@.
data Quantities a
instance Num a => Graded (Quantities a) where
  type Homogeneous (Quantities a) u = Quantity a u
  type Support (Quantities a) = AnyDegree
  MkQuantity x * MkQuantity y = MkQuantity (x Prelude.* y)

square :: forall g u. (Graded g, Support g u) => Homogeneous g u -> Homogeneous g (u *: u)
square x = (*) @g @u @u x x
twice :: forall g u. (Graded g, Support g u, Support g One, Num (Homogeneous g One))
      => Homogeneous g u -> Homogeneous g u
twice x = (*) @g @u @One x 2

-- Code without units that shouldn't need the plugin to typecheck.
squareDouble :: Double
squareDouble = square @Double @One 3
twiceDouble :: Double
twiceDouble = twice @Double @One 3
