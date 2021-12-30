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
import UnitDefs ()

mismatch1 :: Quantity Double [u| s/m |]
mismatch1 = [u| 3 m/s |]

mismatch1_errors :: [[String]]
mismatch1_errors = [ [ "Couldn't match type ‘Base \"s\" /: Base \"m\"’"
                     , "with ‘Base \"m\" /: Base \"s\"’" ]
                   , [ "Couldn't match type ‘Base \"m\" /: Base \"s\"’"
                     , "with ‘Base \"s\" /: Base \"m\"’" ]
                   ]


mismatch2 :: Quantity Int [u| s |]
mismatch2 = [u| 2 m |] +: ([u| 2 s |] :: Quantity Int [u| s |])

mismatch2_errors :: [[String]]
mismatch2_errors = [ [ "Couldn't match type ‘Base \"s\"’ with ‘Base \"m\"’" ]
                   , [ "Couldn't match type ‘Base \"m\"’ with ‘Base \"s\"’" ]
                   ]


given1 :: ((One *: a) ~ (a *: One)) => Quantity Double a -> Quantity Double [u|kg|]
given1 = id

given1_errors :: [[String]]
given1_errors = [ [ "Could not deduce (a ~ Base \"kg\")"
                  , "from the context ((One *: a) ~ (a *: One))" ]
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

{-# ANN opErrorsExpectA_ActualC "HLint: ignore Use camelCase" #-}
opErrorsExpectA_ActualC :: String -> String -> String -> [[String]]
opErrorsExpectA_ActualC a b c =
#if __GLASGOW_HASKELL__ > 710 
  [ [ "Couldn't match type ‘" ++ a ++ "’ with ‘" ++ b ++ "’"
    , "Expected type: Quantity " ++ c ++ " (Base \"m\")"
    ]
  ]
#else
  [ [ "Couldn't match type ‘" ++ b ++ "’ with ‘" ++ a ++ "’"
    ]
  ]
#endif
