{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Data.UnitsOfMeasure.New where

import Data.UnitsOfMeasure
import Data.UnitsOfMeasure.Convert
import Data.UnitsOfMeasure.Internal (Quantity(MkQuantity))

data U_m :: Unit

instance KnownBaseUnit U_m where
  baseUnitName = "m"

instance HasCanonicalBaseUnit U_m

m :: Num a => Quantity a U_m
m = MkQuantity 1


data U_s :: Unit

instance KnownBaseUnit U_s where
  baseUnitName = "s"

s :: Num a => Quantity a U_s
s = MkQuantity 1


data U_in :: Unit

_in :: Num a => Quantity a U_in
_in = MkQuantity 1

instance KnownBaseUnit U_in where
  baseUnitName = "in"

instance HasCanonicalBaseUnit U_in where
  type CanonicalBaseUnit U_in = U_m

  conversionBase _ = 39.3701 *: _in /: m


f :: Quantity a (U_m *: U_s) -> Quantity a (U_s *: U_m)
f x = x

v = f (5 *: s *: m)

y = v /: (2 *: s)

y_in_in = convert y :: Quantity Double U_in


r = print (1 *: _in)