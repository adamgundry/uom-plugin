{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

-- WARNING: It would be a lot of work to add type annotations to avoid type-default
-- warnings and what is more this leads to type checking failures;
--
-- {-# LANGUAGE PartialTypeSignatures #-}
--
--   , testGroup "read normalisation"
--     [ testCase "1 m/m"
--         $ (read "[u| 1 m/m |]" :: _ Integer _) @?= [u| 1 |]
--     , testCase "-0.3 m s^-1"
--         $ (read "[u| -0.3 m s^-1 |]" :: _ Double _) @?= [u| -0.3 m/s |]
--     , testCase "42 s m s"
--         $ (read "[u| 42 s m s |]" :: _ Integer _)  @?= [u| 42 m s^2 |]
--     ]
--
-- > cabal new-repl uom-plugin:units
-- solveSimpleWanteds: too many iterations (limit = 4)
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

#if __GLASGOW_HASKELL__ == 900
-- TODO: it isn't clear why GHC 9.0.1 requires 6 iterations to compile this
-- module, whereas GHC 9.2.0 succeeds with the default of 4.
{-# OPTIONS_GHC -fconstraint-solver-iterations=6 #-}
#endif

module Main
    ( main

    -- * Exported to avoid -Wunused-top-binds.
    , attract
    , foo
    , foo'
    , angularSpeed
    , associativity
    , commutativity
    , unit
    , inverse
    , inverse2
    , f
    , g
    , givens
    , givens2
    , givens3
    , baz
    , baf
    , patternSplice
    , pow
    , dimensionless
    ) where

import Data.UnitsOfMeasure
import Data.UnitsOfMeasure.Convert
import Data.UnitsOfMeasure.Defs ()
import Data.UnitsOfMeasure.Show

import Control.Monad (unless)
import Control.Exception
import Data.List
import Data.Ratio ((%))

import Test.Tasty
import Test.Tasty.HUnit

import Defs ()
import ErrorTests
import Z (z)
import qualified Z (tests)

-- Some basic examples

myMass :: Quantity Double (Base "kg")
myMass = [u| 65 kg |]

gravityOnEarth :: Quantity Double [u| m/s^2 |]
gravityOnEarth = [u| 9.808 m/(s*s) |]

readMass :: Read a => String -> Quantity a (Base "kg")
readMass = fmap [u| kg |] read

forceOnGround :: Quantity Double [u| N |]
forceOnGround = gravityOnEarth *: myMass

inMetresPerSecond :: a -> Quantity a [u| m/s |]
inMetresPerSecond = [u| m/s |]

attract
    :: Fractional a
    => Quantity a [u| kg |]
    -> Quantity a [u| kg |]
    -> Quantity a [u| m |]
    -> Quantity a [u| N |]
attract
    (m1 :: Quantity a [u| kg |])
    (m2 :: Quantity a [u| kg |])
    (r :: Quantity a [u| m |])
    = _G *: m1 *: m2 /: (r *: r) :: Quantity a [u| N |]
  where
    _G = [u| 6.67384e-11 N*m^2/kg^2 |]

sum' :: [Quantity Double u] -> Quantity Double u
sum' = foldr (+:) zero

mean :: [Quantity Double u] -> Quantity Double u
mean xs = sum' xs /: mk (genericLength xs)

foo :: Num a => Quantity a u -> Quantity a v -> Quantity a (u *: v)
foo x y = x *: y +: y *: x

foo' :: Num a => Quantity a u -> Quantity a v -> Quantity a (u *: v)
foo' = foo

-- thanks to expipiplus1, https://github.com/adamgundry/uom-plugin/issues/14
angularSpeed :: Quantity Rational [u|rad/s|]
angularSpeed = z x
  where x :: Quantity Rational [u|s^-1|]
        x = undefined


-- Check that the abelian group laws hold

associativity :: Quantity a (u *: (v *: w)) -> Quantity a ((u *: v) *: w)
associativity = id

commutativity :: Quantity a (u *: v) -> Quantity a (v *: u)
commutativity = id

unit :: Quantity a (u *: One) -> Quantity a u
unit = id

inverse :: Quantity a (u *: (One /: u)) -> Quantity a One
inverse = id

inverse2 :: proxy b -> Quantity a (Base b /: Base b) -> Quantity a One
inverse2 _ = id


-- Gingerly now...

-- w^-2 ~ kg^-2  =>  w ~ kg
f :: (One /: (w ^: 2)) ~ (One /: [u| kg^2 |])  => Quantity a w -> Quantity a [u| kg |]
f = id

-- u ~ v * w, v^2 ~ v  =>  u ~ w
g :: (u ~ (v *: w), (v ^: 2) ~ v) => Quantity a u -> Quantity a w
g = id

-- a*a ~ 1  =>  a ~ 1
givens :: ((a *: a) ~ One) => Quantity Double a -> Quantity Double One
givens = id

-- a^2 ~ b^3, b^6 ~ 1 => a ~ 1
givens2 :: ((a ^: 2) ~ (b ^: 3), (b ^: 6) ~ One) => Quantity Double a -> Quantity Double One
givens2 = id

-- a^2 ~ b^3, b^37 ~ 1 => b ~ 1
givens3 :: ((a ^: 2) ~ (b ^: 3), (b ^: 37) ~ One) => Quantity Double b -> Quantity Double One
givens3 = id

-- in baf, c is uniquely determined to be a^3 (or b^2)
baz :: (a ~ (c ^: 3), b ~ (c ^: 2)) => Quantity Double a -> Quantity Double b -> Quantity Double c -> Int
baz _ _ _ = 3
baf :: ((a ^: 2) ~ (b ^: 3)) => Quantity Double a -> Quantity Double b -> Int
baf qa qb = baz qa qb undefined


-- Miscellaneous bits and bobs

-- Pattern splices are supported, albeit with restricted types
patternSplice :: Quantity Integer [u| m |] -> Quantity Rational [u| kg/s |] -> Bool
patternSplice [u| 2 m |] [u| 0.0 kg / s |] = True
patternSplice [u| 1 m |] [u| 0.1 kg / s |] = True
patternSplice _          _                 = False

-- Andrew's awkward generalisation example is accepted only with a
-- type signature, even with NoMonoLocalBinds
tricky
    :: forall a u . Num a
    => Quantity a u
    -> (Quantity a (u *: Base "m"), Quantity a (u *: Base "kg"))
tricky x =
    let h :: Quantity a v -> Quantity a (u *: v)
        h = (x *:)
    in (h [u| 3 m |], h [u| 5 kg |])


-- Test that basic constraints involving exponentiation work
pow :: Quantity a (u *: (v ^: i)) -> Quantity a ((v ^: i) *: u)
pow = id


-- This declares a synonym for One
[u| dimensionless = 1 |]
dimensionless :: Quantity a [u|dimensionless|] -> Quantity a [u|1|]
dimensionless = id


-- Runtime testsuite

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "uom-plugin"
  [ testGroup "Get the underlying value with unQuantity"
    [ testCase "unQuantity 3 m"                $ unQuantity [u| 3 m |]            @?= 3
    , testCase "unQuantity 3 s^2"              $ unQuantity [u| 3 s^2 |]          @?= 3
#if __GLASGOW_HASKELL__ > 802
    -- TODO: Find out why unQuantity (3 m s^-1) fails with ghc-8.0.2.
    -- solveSimpleWanteds: too many iterations (limit = 4)
    , testCase "unQuantity 3 m s^-1"           $ unQuantity [u| 3 m s^-1 |]       @?= 3
#endif
    , testCase "unQuantity 3.0 kg m^2 / m s^2" $ unQuantity [u| 3.0 kg m / s^2 |] @?= 3
    , testCase "unQuantity 1"                  $ unQuantity (mk 1)                @?= 1
    , testCase "unQuantity 1 (1/s)"            $ unQuantity [u| 1 (1/s) |]        @?= 1
    , testCase "unQuantity 1 1/s"              $ unQuantity [u| 1 1/s |]          @?= 1
    , testCase "unQuantity 1 s^-1"             $ unQuantity [u| 1 s^-1 |]         @?= 1
    , testCase "unQuantity 2 1 / kg s"         $ unQuantity [u| 2 1 / kg s |]     @?= 2
    , testCase "unQuantity (1 % 2) kg"         $ unQuantity [u| 1 % 2 kg |]       @?= 0.5
    ]
  , testGroup "Attach units by applying the quasiquoter without a numeric value"
    [ testCase "m 3"                           $ [u| m |] 3           @?= [u| 3 m |]
    , testCase "m <$> [3..5]"                  $ [u| m |] <$> [3..5]  @?= [[u| 3 m |],[u| 4 m |],[u| 5 m |]]
    , testCase "m/s 3"                         $ [u| m/s |] 3         @?= [u| 3 m/s |]
#if __GLASGOW_HASKELL__ > 802
    -- TODO: Find out why (m s^-1 3) fails with ghc-8.0.2.
    -- solveSimpleWanteds: too many iterations (limit = 4)
    , testCase "m s^-1 3"                      $ [u| m s^-1 |] 3      @?= [u| 3 m s^-1 |]
#endif
    , testCase "s^2 3"                         $ [u| s^2 |] 3         @?= [u| 3 s^2 |]
    , testCase "1 $ 3"                         $ [u|dimensionless|] 3 @?= [u| 3 |]
    , testCase "fmap [u| kg |] read $ \"3\""   $ readMass "3"         @?= [u| 3 kg |]
    , testCase "fmap [u| kg |] read $ \"3.0\"" $ readMass "3"         @?= [u| 3.0 kg |]
    ]
  , testGroup "Showing constants"
    [ testCase "show 3m"                 $ show [u| 3 m |]                @?= "[u| 3 m |]"
    , testCase "show 3m/s"               $ show [u| 3 m/s |]              @?= "[u| 3 m / s |]"
    , testCase "show 3.2 s^2"            $ show [u| 3.2 s^2 |]            @?= "[u| 3.2 s^2 |]"
    , testCase "show 3.0 kg m^2 / m s^2" $ show [u| 3.0 kg m^2 / m s^2 |] @?= "[u| 3.0 kg m / s^2 |]"
    , testCase "show 1"                  $ show (mk 1)                    @?= "[u| 1 |]"
    , testCase "show 1 s^-1"             $ show [u| 1 s^-1 |]             @?= "[u| 1 s^-1 |]"
    , testCase "show 2 1 / kg s"         $ show [u| 2 1 / kg s |]         @?= "[u| 2 kg^-1 s^-1 |]"
    , testCase "show (1 % 2) kg"         $ show [u| 1 % 2 kg |]           @?= "[u| 0.5 kg |]"
    ]
  , testGroup "Basic operations"
    [ testCase "2 + 2"                   $ [u| 2 s |] +: [u| 2 s |]        @?= [u| 4 s |]
    , testCase "in m/s"                  $ inMetresPerSecond 5             @?= [u| 5 m/s |]
    , testCase "mean"                    $ mean [ [u| 2 N |], [u| 4 N |] ] @?= [u| 3 N |]
    , testCase "tricky generalisation"   $ tricky [u| 2 s |]               @?= ([u| 6 m s |], [u| 10 kg s |])
    , testCase "polymorphic zero"        $ [u| 0 |] @?= [u| 0 m |]
    , testCase "polymorphic frac zero"   $ [u| 0.0 |] @?= [u| 0.0 N / m |]
    ]
  , testGroup "Literal 1 (*:) Quantity _ u"
    [ testCase "_ = Double"
        $ 1 *: ([u| 1 m |] :: (Quantity Double (Base "m"))) @?= [u| 1 m |]
    , testCase "_ = Int"
        $ 1 *: ([u| 1 m |] :: (Quantity Int (Base "m"))) @?= [u| 1 m |]
    , testCase "_ = Integer"
        $ 1 *: ([u| 1 m |] :: (Quantity Integer (Base "m"))) @?= [u| 1 m |]
    , testCase "_ = Rational, 1 *: [u| 1 m |]"
        $ 1 *: ([u| 1 m |] :: (Quantity Rational (Base "m"))) @?= [u| 1 m |]
    , testCase "_ = Rational, mk (1 % 1) *: [u| 1 m |]"
        $ mk (1 % 1) *: ([u| 1 m |] :: (Quantity Rational (Base "m"))) @?= [u| 1 m |]
    , testCase "_ = Rational, 1 *: [u| 1 % 1 m |]"
        $ 1 *: ([u| 1 % 1 m |] :: (Quantity Rational (Base "m"))) @?= [u| 1 m |]
    , testCase "_ = Rational, mk (1 % 1) *: [u| 1 % 1 m |]"
        $ mk (1 % 1) *: ([u| 1 % 1 m |] :: (Quantity Rational (Base "m"))) @?= [u| 1 m |]
    ]
  , testGroup "(1 :: Quantity _ One) (*:) Quantity _ u"
    [ testCase "_ = Double"
        $ (1 :: Quantity Double One) *: ([u| 1 m |] :: (Quantity Double (Base "m"))) @?= [u| 1 m |]
    , testCase "_ = Int"
        $ (1 :: Quantity Int One) *: ([u| 1 m |] :: (Quantity Int (Base "m"))) @?= [u| 1 m |]
    , testCase "_ = Integer"
        $ (1 :: Quantity Integer One) *: ([u| 1 m |] :: (Quantity Integer (Base "m"))) @?= [u| 1 m |]
    , testCase "_ = Int"
        $ (1 :: Quantity Rational One) *: ([u| 1 m |] :: (Quantity Rational (Base "m"))) @?= [u| 1 m |]
    ]
  , testGroup "errors when a /= b, (1 :: Quantity a One) (*:) Quantity b u"
    [ testGroup "b = Double"
      [ testCase "a = Int" $ op_a1 `throws` opErrors "Double" "Int" "Int"
      , testCase "a = Integer" $ op_a2 `throws` opErrors "Double" "Integer" "Integer"
      , testCase "a = Rational" $ op_a3 `throws` opErrors "Double" "GHC.Real.Ratio Integer" "Rational"
      ]
    , testGroup "b = Int"
      [ testCase "a = Double" $ op_b1 `throws` opErrors "Int" "Double" "Double"
      , testCase "a = Integer" $ op_b2 `throws` opErrors "Int" "Integer" "Integer"
      , testCase "a = Rational" $ op_b3 `throws` opErrors "Int" "GHC.Real.Ratio Integer" "Rational"
      ]
    , testGroup "b = Integer"
      [ testCase "a = Double" $ op_c1 `throws` opErrors "Integer" "Double" "Double"
      , testCase "a = Int" $ op_c2 `throws` opErrors "Integer" "Int" "Int"
      , testCase "a = Rational" $ op_c3 `throws` opErrors "Integer" "GHC.Real.Ratio Integer" "Rational"
      ]
    , testGroup "b = Rational"
      [ testCase "a = Double" $ op_d1 `throws` opErrors "GHC.Real.Ratio Integer" "Double" "Double"
      , testCase "a = Int" $ op_d2 `throws` opErrors "GHC.Real.Ratio Integer" "Int" "Int"
      , testCase "a = Integer" $ op_d3 `throws` opErrors "GHC.Real.Ratio Integer" "Integer" "Integer"
      ]
    ]
  , testGroup "showQuantity"
    [ testCase "myMass"         $ showQuantity myMass         @?= "65.0 kg"
    , testCase "gravityOnEarth" $ showQuantity gravityOnEarth @?= "9.808 m / s^2"
    , testCase "forceOnGround"  $ showQuantity forceOnGround  @?= "637.52 kg m / s^2"
    ]
  , testGroup "convert"
    [ testCase "10m in ft"     $ convert [u| 10m |]   @?= [u| 32.8 ft |]
    , testCase "5 km^2 in m^2" $ convert [u| 5km^2 |] @?= [u| 5000000 m m |]
    , testCase "ratio"         $ show (ratio [u| ft |] [u| m |]) @?= "[u| 3.28 ft / m |]"
    , testCase "100l in m^3"   $ convert [u| 100l |]   @?= [u| 0.1 m^3 |]
    , testCase "1l/m in m^2"   $ convert [u| 1l/m |]   @?= [u| 0.001 m^2 |]
    , testCase "1l/m in m^2"   $ convert [u| 1l/m |]   @?= [u| 0.001 m^2 |]
    , testCase "5l in ft^3"    $ convert [u| 5l   |]   @?= [u| 0.17643776 ft^3 |]
    , testCase "2000000l^2 in ft^3 m^3" $ convert [u| 2000000l^2 |] @?= [u| 70.575104 ft^3 m^3 |]
    , testCase "42 rad/s in s^-1" $ convert [u| 42 rad/s |] @?= [u| 42 s^-1 |]
    , testCase "2.4 l/h in m" $ convert [u| 2.4 l/ha |] @?= [u| 2.4e-7 m |]
    , testCase "1 m^4 in l m" $ convert [u| 1 m^4 |] @?= [u| 1000 l m |]
    ]
  , Z.tests
  , testGroup "errors"
    [ testCase "s/m ~ m/s"            $ mismatch1 `throws` mismatch1_errors
    , testCase "m + s"                $ mismatch2 `throws` mismatch2_errors
    , testCase "a ~ a  =>  a ~ kg"    $ given1 undefined `throws` given1_errors
    , testCase "a ~ b  =>  a ~ kg"    $ given2 undefined `throws` given2_errors
    , testCase "a^2 ~ b^3  =>  a ~ s" $ given3 undefined `throws` given3_errors
    ]
  , testGroup "read . show"
    [ testCase "3 m"     $ read (show [u| 3 m     |]) @?= [u| 3 m     |]
    , testCase "1.2 m/s" $ read (show [u| 1.2 m/s |]) @?= [u| 1.2 m/s |]
    , testCase "0"       $ read (show [u| 1       |]) @?= [u| 1       |]
    ]
  , testGroup "read normalisation"
    [ testCase "1 m/m"       $ read "[u| 1 m/m |]"       @?= [u| 1 |]
    , testCase "-0.3 m s^-1" $ read "[u| -0.3 m s^-1 |]" @?= [u| -0.3 m/s |]
    , testCase "42 s m s"    $ read "[u| 42 s m s |]"    @?= [u| 42 m s^2 |]
    ]
  , testGroup "read equality (avoid false equivalences)"
    [ testCase "1 m/m^2 /= 1 m" $
        (read "[u| 1 m/m^2 |]" :: Quantity Double [u| m |]) `throws` noParse

    , testCase "1 m /= 1 m/m^2" $
        (read "[u| 1 m |]" :: Quantity Double [u| m/m^2 |]) `throws` noParse
    ]
  ]

-- | Assert that evaluation of the first argument (to WHNF) will throw
-- an exception whose string representation contains one of the given
-- lists of substrings.
throws :: a -> [[String]] -> Assertion
throws v xs =
    (evaluate v >> assertFailure "No exception!") `catch` \ (e :: SomeException) ->
        unless (any (all (`isInfixOf` show e)) xs) $
          assertFailure ("Expected:\n" ++ unlines (concat xs) ++ "\nbut got:\n" ++ show e)

noParse :: [[String]]
noParse = [["Prelude.read: no parse"]]
