{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
module Examples where

import Data.UnitsOfMeasure
import Data.UnitsOfMeasure.TH

import Data.List


-- Declaring some base units and derived units
[u| ft, kg, m, s,
    N = kg * m/s^2
  |]

-- An integer constant quantity with units
myMass = [u| 65 kg |]

-- A rational constant, this time with a type signature
gravityOnEarth :: Quantity Double [u| m/s^2 |]
gravityOnEarth = [u| 9.808 m/(s*s) |]

-- Multiplication of quantities; the typechecker plugin automatically
-- solves the constraint
--
--  N ~ kg * m/s^2 ~ (m/s^2) * kg
forceOnGround :: Quantity Double [u| N |]
forceOnGround = gravityOnEarth *: myMass

-- A constructor for quanties with fixed units (there is no polite way
-- to construct `Quantity a u` for polymorphic `u`)
inMetresPerSecond :: a -> Quantity a [u| m/s |]
inMetresPerSecond = [u| m/s |]


-- A different way of writing unit signatures
attract (m1 :: Quantity a [u| kg |]) (m2 :: Quantity a [u| kg |]) (r :: Quantity a [u| m |])
    = _G *: m1 *: m2 /: (r *: r) :: Quantity a [u| N |]
  where
    _G = [u| 6.67384e-11 N*m^2/kg^2 |]


-- Some polymorphic functions for working with quantities
sum' xs = foldr (+:) zero xs
mean xs = sum' xs /: mk (genericLength xs)
foo x y = x *: y +: y *: x


-- Some given equations to make use of
f :: (One /: (w ^: 2)) ~ (One /: [u| kg^2 |])  => Quantity a w -> Quantity a [u| kg |]
f = id

g :: (u ~ (v *: w), (v ^: 2) ~ v) => Quantity a u -> Quantity a w
g = id


-- We can use standard typeclass methods on quantities
x = show [u| 3 m |]
