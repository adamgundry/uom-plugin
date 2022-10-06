{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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

#if __GLASGOW_HASKELL__ >= 902
-- See https://github.com/adamgundry/uom-plugin/pull/86.  This code works in GHC
-- 9.2 and later because they do not flatten, but is broken in 9.0 because of
-- flattening. For now we skip testing it in 9.0.  In principle we should be
-- able to fix it by having simplify-givens do substitution.
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
#else
deriving instance (q ~ Quantity Double u, Show q) => Show (A q)
deriving instance (q ~ Quantity Double u, Show q) => Show (B q)
#endif

tests :: TestTree
tests = testGroup "show via convert"
    [ testCase "A 1.01km" $ show (A [u| 1.01 km |]) @?= "[u| 1010.0 m |]"
    , testCase "B 1010m" $ show (B [u| 1010.0 m |]) @?= "[u| 1010.0 m |]"
    ]
