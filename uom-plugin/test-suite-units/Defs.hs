{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Defs where

import Data.UnitsOfMeasure
import Data.UnitsOfMeasure.Defs

-- Declarations.
declareBaseUnit "byte"
declareDerivedUnit "bps" "byte / s"
declareConvertibleUnit "kilobyte" 1024 "byte"
declareConvertibleUnit "squiggle" 2 "m/s"

-- This declares a dimensionless unit that requires explicit conversion.
[u| dime = 1 1 |]
dime :: Fractional a => Quantity a [u|dime|] -> Quantity a [u|1|]
dime = convert


try :: Quantity Double [u| ft^3 m^3 |]
try = convert [u| 2 l^2  |]



{-

-- These tests demonstrate the need for simplification of givens.  This doesn't
-- currently work, however.

type family F (u :: Unit)

thingy :: (x *: y ~ Base "m") => proxy x y -> F x -> F (Base "m" /: y)
thingy _ x = x

thingy2 :: (x *: x ~ x) => proxy x -> F x -> F One
thingy2 _ x = x

thingy3 :: (x *: x ~ x *: x *: x) => proxy x -> F x -> F One
thingy3 _ x = x

-}
