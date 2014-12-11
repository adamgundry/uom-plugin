{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

import Data.UnitsOfMeasure
import Data.UnitsOfMeasure.Convert
import Data.UnitsOfMeasure.Show
import Data.UnitsOfMeasure.TH

import Data.List

import Test.Tasty
import Test.Tasty.HUnit

-- Declaring some base units and derived units
[u| ft, kg, m, s, km,
    N = kg * m/s^2
  |]

-- Conversions
instance HasCanonicalBaseUnit "s"

instance HasCanonicalBaseUnit "m"

instance HasCanonicalBaseUnit "ft" where
  type CanonicalBaseUnit "ft" = "m"
  conversionBase _ = [u| 3.28 ft/m |]

instance HasCanonicalBaseUnit "km" where
  type CanonicalBaseUnit "km" = "m"
  conversionBase _ = [u| 0.001 km/m |]


myMass :: Quantity Double (Base "kg")
myMass = [u| 65 kg |]

gravityOnEarth :: Quantity Double [u| m/s^2 |]
gravityOnEarth = [u| 9.808 m/(s*s) |]

forceOnGround :: Quantity Double [u| N |]
forceOnGround = gravityOnEarth *: myMass

inMetresPerSecond :: a -> Quantity a [u| m/s |]
inMetresPerSecond = [u| m/s |]

attract (m1 :: Quantity a [u| kg |]) (m2 :: Quantity a [u| kg |]) (r :: Quantity a [u| m |])
    = _G *: m1 *: m2 /: (r *: r) :: Quantity a [u| N |]
  where
    _G = [u| 6.67384e-11 N*m^2/kg^2 |]

sum' xs = foldr (+:) zero xs
mean xs = sum' xs /: mk (genericLength xs)
foo x y = x *: y +: y *: x

f :: (One /: (w ^: 2)) ~ (One /: [u| kg^2 |])  => Quantity a w -> Quantity a [u| kg |]
f = id

g :: (u ~ (v *: w), (v ^: 2) ~ v) => Quantity a u -> Quantity a w
g = id


associativity :: Quantity a (u *: (v *: w)) -> Quantity a ((u *: v) *: w)
associativity = id

commutativity :: Quantity a (u *: v) -> Quantity a (v *: u)
commutativity = id

unit :: Quantity a (u *: One) -> Quantity a u
unit = id

inverse :: Quantity a (u *: (One /: u)) -> Quantity a One
inverse = id

-- Inferring this type leads to unit equations with occur-check failures
z q = convert q


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "uom-plugin"
  [ testGroup "Basic functionality"
    [ testCase "show" $ show [u| 3 m |] @?= "MkQuantity 3"
    ]
  , testGroup "showQuantity"
    [ testCase "myMass"         $ showQuantity myMass         @?= "65.0 kg"
    , testCase "gravityOnEarth" $ showQuantity gravityOnEarth @?= "9.808 m s^-2"
    , testCase "forceOnGround"  $ showQuantity forceOnGround  @?= "637.52 kg m s^-2"
    ]
  , testGroup "convert"
    [ testCase "10m in ft"     $ unQuantity (convert [u| 10m |] :: Quantity Double [u| ft |]) @?= 32.8
    , testCase "5 km^2 in m^2" $ unQuantity (convert [u| 5km^2 |] :: Quantity Double [u| m*m |]) @?= 5000000
    ]
  ]
