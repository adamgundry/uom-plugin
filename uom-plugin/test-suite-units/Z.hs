{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Z (z, tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.UnitsOfMeasure (Quantity, u)
import Data.UnitsOfMeasure.Convert (Convertible, convert)
import Data.UnitsOfMeasure.Defs ()


-- Inferring this type used to lead to unit equations with occur-check
-- failures, because it involves things like Pack (Unpack u) ~ u
-- The type signature is intentionally left off here to check that the
-- compiler can infer it.
-- z :: forall a (u :: Unit) (v :: Unit). (Fractional a, Convertible u v)
--   => Quantity a u
--   -> Quantity a v
{-# ANN z "HLint: ignore Eta reduce" #-}
z q = convert q

newtype A a = A a
newtype B a = B a

instance (Convertible u [u| m |], q ~ Quantity Double u) => Show (A q) where
    show (A x) = show y
        where
            y :: Quantity Double [u| m |]
            y = convert x

instance (q ~ Quantity Double [u| m |]) => Show (B q) where
    show (B x) = show y
        where
            y :: Quantity Double [u| m |]
            y = convert x

tests :: TestTree
tests = testGroup "show via convert"
    [ testCase "A 1.01km" $ show (A [u| 1.01 km |]) @?= "[u| 1010.0 m |]"
    , testCase "B 1010m" $ show (B [u| 1010.0 m |]) @?= "[u| 1010.0 m |]"
    ]