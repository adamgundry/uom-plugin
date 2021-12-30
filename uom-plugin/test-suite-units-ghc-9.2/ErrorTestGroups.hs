{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module ErrorTestGroups (errorsWhenTestGroup, errorsTestGroup, throws) where

import Control.Monad (unless)
import Control.Exception
import Data.List

import Test.Tasty
import Test.Tasty.HUnit

import UnitDefs ()
import UnitDefsTests ()
import ErrorTests

errorsWhenTestGroup :: TestTree
errorsWhenTestGroup =
  testGroup "errors when a /= b, (1 :: Quantity a One) (*:) Quantity b u"
    [ testGroup "b = Double"
      [ testCase "a = Int" $ op_a1 `throws` opErrorsExpectA_ActualC "Double" "Int" "Int"
      , testCase "a = Integer" $ op_a2 `throws` opErrorsExpectA_ActualC "Double" "Integer" "Integer"
      , testCase "a = Rational" $ op_a3 `throws` opErrorsExpectA_ActualC "Double" "GHC.Real.Ratio Integer" "Rational"
      ]
    , testGroup "b = Int"
      [ testCase "a = Double" $ op_b1 `throws` opErrorsExpectA_ActualC "Int" "Double" "Double"
      , testCase "a = Integer" $ op_b2 `throws` opErrorsExpectA_ActualC "Int" "Integer" "Integer"
      , testCase "a = Rational" $ op_b3 `throws` opErrorsExpectA_ActualC "Int" "GHC.Real.Ratio Integer" "Rational"
      ]
    , testGroup "b = Integer"
      [ testCase "a = Double" $ op_c1 `throws` opErrorsExpectA_ActualC "Integer" "Double" "Double"
      , testCase "a = Int" $ op_c2 `throws` opErrorsExpectA_ActualC "Integer" "Int" "Int"
      , testCase "a = Rational" $ op_c3 `throws` opErrorsExpectA_ActualC "Integer" "GHC.Real.Ratio Integer" "Rational"
      ]
    , testGroup "b = Rational"
      [ testCase "a = Double" $ op_d1 `throws` opErrorsExpectC_ActualB "GHC.Real.Ratio Integer" "Double" "Rational"
      , testCase "a = Int" $ op_d2 `throws` opErrorsExpectC_ActualB "GHC.Real.Ratio Integer" "Int" "Rational"
      , testCase "a = Integer" $ op_d3 `throws` opErrorsExpectC_ActualB "GHC.Real.Ratio Integer" "Integer" "Rational"
      ]
    ]

errorsTestGroup :: TestTree
errorsTestGroup =
  testGroup "errors"
    [ testCase "s/m ~ m/s"            $ mismatch1 `throws` mismatch1_errors
    , testCase "m + s"                $ mismatch2 `throws` mismatch2_errors
    , testCase "a ~ a  =>  a ~ kg"    $ given1 undefined `throws` given1_errors
    , testCase "a ~ b  =>  a ~ kg"    $ given2 undefined `throws` given2_errors
    , testCase "a^2 ~ b^3  =>  a ~ s" $ given3 undefined `throws` given3_errors
    ]

-- | Assert that evaluation of the first argument (to WHNF) will throw
-- an exception whose string representation contains one of the given
-- lists of substrings.
throws :: a -> [[String]] -> Assertion
throws v xs =
    (evaluate v >> assertFailure "No exception!") `catch` \ (e :: SomeException) ->
        unless (any (all (`isInfixOf` show e)) xs) $ throw e
