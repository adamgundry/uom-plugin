{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

#if __GLASGOW_HASKELL__ > 710
{-# OPTIONS_GHC -fno-warn-deferred-type-errors #-}
#endif

module ErrorTests where

import Data.UnitsOfMeasure
import Data.UnitsOfMeasure.Defs ()

import GHC.TypeLits

mismatch1 :: Quantity Double [u| s/m |]
mismatch1 = [u| 3 m/s |]

mismatch1_errors :: [[String]]
mismatch1_errors = couldn'tMatchErrors "Base \"m\" /: Base \"s\"" "Base \"s\" /: Base \"m\""

mismatch2 :: Quantity Int [u| s |]
mismatch2 = [u| 2 m |] +: ([u| 2 s |] :: Quantity Int [u| s |])

mismatch2_errors :: [[String]]
mismatch2_errors = couldn'tMatchErrors "Base \"s\"" "Base \"m\""

couldn'tMatchErrors :: String -> String -> [[String]]
couldn'tMatchErrors t1 t2 =
    [ [ "Couldn't match type ‘" ++ t1 ++ "’", "with ‘" ++ t2 ++ "’" ]
    , [ "Couldn't match type ‘" ++ t2 ++ "’", "with ‘" ++ t1 ++ "’" ]
    , [ "Couldn't match type: " ++ t1, "with: " ++ t2 ]
    , [ "Couldn't match type: " ++ t2, "with: " ++ t1 ]
    ]


given1 :: ((One *: a) ~ (a *: One)) => Quantity Double a -> Quantity Double [u|kg|]
given1 = id

given1_errors :: [[String]]
given1_errors = [ [ "Could not deduce (a ~ Base \"kg\")"
                  , "from the context ((One *: a) ~ (a *: One))" ]
                , [ "Could not deduce (Base \"kg\" ~ a)"
                  , "from the context: (One *: a) ~ (a *: One)" ]
                , [ "Could not deduce: a ~ Base \"kg\""
                  , "from the context: (One *: a) ~ (a *: One)" ]
                , [ "Could not deduce: Base \"kg\" ~ a"
                  , "from the context: (One *: a) ~ (a *: One)" ]
                ]


given2 :: ((One *: a) ~ (b *: One)) => Quantity Double a -> Quantity Double [u|kg|]
given2 = id

given2_errors :: [[String]]
given2_errors = [ [ "Could not deduce (a ~ Base \"kg\")"
                  , "from the context ((One *: a) ~ (b *: One))" ]
                , [ "Could not deduce (Base \"kg\" ~ a)"
                  , "from the context: (One *: a) ~ (b *: One)" ]
                , [ "Could not deduce: a ~ Base \"kg\""
                  , "from the context: (One *: a) ~ (b *: One)" ]
                , [ "Could not deduce: Base \"kg\" ~ a"
                  , "from the context: (One *: a) ~ (b *: One)" ]
                ]


given3 :: ((a ^: 2) ~ (b ^: 3)) => Quantity Integer b -> Quantity Integer a
given3 _ = [u| 3 s |]

given3_errors :: [[String]]
given3_errors = [ [ "Could not deduce (a ~ Base \"s\")"
                  , "from the context ((a ^: 2) ~ (b ^: 3))" ]
                , [ "Could not deduce (Base \"s\" ~ a)"
                  , "from the context: (a ^: 2) ~ (b ^: 3)" ]
                , [ "Could not deduce: a ~ Base \"s\""
                  , "from the context: (a ^: 2) ~ (b ^: 3)" ]
                , [ "Could not deduce: Base \"s\" ~ a"
                  , "from the context: (a ^: 2) ~ (b ^: 3)" ]
                ]

op_a1 :: Quantity Double [u| m |]
op_a1 = (1 :: Quantity Int One) *: ([u| 1 m |] :: (Quantity Double (Base "m")))

op_a2 :: Quantity Double [u| m |]
op_a2 = (1 :: Quantity Integer One) *: ([u| 1 m |] :: (Quantity Double (Base "m")))

op_a3 :: Quantity Double [u| m |]
op_a3 = (1 :: Quantity Rational One) *: ([u| 1 m |] :: (Quantity Double (Base "m")))

op_b1 :: Quantity Int [u| m |]
op_b1 = (1 :: Quantity Double One) *: ([u| 1 m |] :: (Quantity Int (Base "m")))

op_b2 :: Quantity Int [u| m |]
op_b2 = (1 :: Quantity Integer One) *: ([u| 1 m |] :: (Quantity Int (Base "m")))

op_b3 :: Quantity Int [u| m |]
op_b3 = (1 :: Quantity Rational One) *: ([u| 1 m |] :: (Quantity Int (Base "m")))

op_c1 :: Quantity Integer [u| m |]
op_c1 = (1 :: Quantity Double One) *: ([u| 1 m |] :: (Quantity Integer (Base "m")))

op_c2 :: Quantity Integer [u| m |]
op_c2 = (1 :: Quantity Int One) *: ([u| 1 m |] :: (Quantity Integer (Base "m")))

op_c3 :: Quantity Integer [u| m |]
op_c3 = (1 :: Quantity Rational One) *: ([u| 1 m |] :: (Quantity Integer (Base "m")))

op_d1 :: Quantity Rational [u| m |]
op_d1 = (1 :: Quantity Double One) *: ([u| 1 m |] :: (Quantity Rational (Base "m")))

op_d2 :: Quantity Rational [u| m |]
op_d2 = (1 :: Quantity Int One) *: ([u| 1 m |] :: (Quantity Rational (Base "m")))

op_d3 :: Quantity Rational [u| m |]
op_d3 = (1 :: Quantity Integer One) *: ([u| 1 m |] :: (Quantity Rational (Base "m")))

opErrors :: String -> String -> String -> [[String]]
opErrors a b c = matchErrors a b c "One"

matchErrors :: String -> String -> String -> String -> [[String]]
matchErrors a b c d =
#if __GLASGOW_HASKELL__ >= 900
  [ [ "Couldn't match type ‘" ++ a ++ "’ with ‘" ++ b ++ "’"
    , "Actual: Quantity " ++ c ++ " " ++ d
    ]
  , [ "Couldn't match type ‘" ++ a ++ "’ with ‘" ++ b ++ "’"
    , "Expected: Quantity " ++ c ++ " " ++ d
    ]
  , [ "Couldn't match type ‘" ++ b ++ "’ with ‘" ++ a ++ "’"
    , "Expected: Quantity " ++ c ++ " " ++ d
    ]
  , [ "Couldn't match type ‘" ++ b ++ "’ with ‘" ++ a ++ "’"
    , "Actual: Quantity " ++ c ++ " " ++ d
    ]
  , [ "Couldn't match type: " ++ a, "with: " ++ b
    , "Actual: Quantity " ++ c ++ " " ++ d
    ]
  ]
#else
  [ [ "Couldn't match type ‘" ++ b ++ "’ with ‘" ++ a ++ "’"
    ]
  ]
#endif


exponent_doesn't_distribute :: Quantity Double ([u| m |] ^: (x + y)) -> Quantity Double (([u| m |] ^: x) *: [u| m |] ^: y)
exponent_doesn't_distribute x = x