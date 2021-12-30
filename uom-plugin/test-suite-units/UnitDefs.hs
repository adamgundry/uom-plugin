{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

-- | This module exports some example definitions of base and derived
-- units, for demonstration purposes.  In the future, this is likely
-- to change or be moved to a separate package.
module UnitDefs () where

import Data.UnitsOfMeasure.TH

-- | The SI base units
-- http://www.bipm.org/en/measurement-units/
-- >>> type instance MkUnit "m" = Base "m"
-- >>> instance HasCanonicalBaseUnit "m"
-- >>> type instance MkUnit "kg" = Base "kg"
-- >>> instance HasCanonicalBaseUnit "kg"
-- >>> type instance MkUnit "s" = Base "s"
-- >>> instance HasCanonicalBaseUnit "s"
-- >>> type instance MkUnit "A" = Base "A"
-- >>> instance HasCanonicalBaseUnit "A"
-- >>> type instance MkUnit "K" = Base "K"
-- >>> instance HasCanonicalBaseUnit "K"
-- >>> type instance MkUnit "mol" = Base "mol"
-- >>> instance HasCanonicalBaseUnit "mol"
-- >>> type instance MkUnit "cd" = Base "cd"
[u| m, kg, s, A, K, mol, cd |]

-- | Some prefixed units
-- >>> type instance MkUnit "km" = Base "km"
-- >>> instance HasCanonicalBaseUnit "km" where
-- >>>   type CanonicalBaseUnit "km" = MkUnit "m"
-- >>>   conversionBase _ = MkQuantity 1.0e-3
-- >>> type instance MkUnit "g" = Base "g"
-- >>> instance HasCanonicalBaseUnit "g" where
-- >>>   type CanonicalBaseUnit "g" = MkUnit "kg"
-- >>>   conversionBase _ = MkQuantity 1000.0
[u| km = 1000m, g = 0.001 kg |]

-- | SI derived units
-- http://physics.nist.gov/cuu/Units/units.html
-- >>> type instance MkUnit "Hz" = (/:) One ((^:) (MkUnit "s") 1)
-- >>> type instance MkUnit "N" = (/:) ((*:) (MkUnit "kg") (MkUnit "m")) ((^:) (MkUnit "s") 2)
-- >>> type instance MkUnit "Pa" = (/:) (MkUnit "N") ((^:) (MkUnit "m") 2)
-- >>> type instance MkUnit "J" = (*:) (MkUnit "N") (MkUnit "m")
-- >>> type instance MkUnit "W" = (/:) (MkUnit "J") (MkUnit "s")
-- >>> type instance MkUnit "C" = (*:) (MkUnit "s") (MkUnit "A")
-- >>> type instance MkUnit "V" = (/:) (MkUnit "W") (MkUnit "A")
-- >>> type instance MkUnit "F" = (/:) (MkUnit "C") (MkUnit "V")
-- >>> type instance MkUnit "ohm" = (/:) (MkUnit "V") (MkUnit "A")
[u| Hz = s^-1
  , N  = kg m / s^2
  , Pa = N / m^2
  , J  = N m
  , W  = J / s
  , C  = s A
  , V  = W / A
  , F  = C / V
  , ohm = V / A
 |]

-- | SI derived units that require explicit conversion
-- >>> type instance MkUnit "rad" = Base "rad"
-- >>> instance HasCanonicalBaseUnit "rad" where
-- >>>   type CanonicalBaseUnit "rad" = One
-- >>>   conversionBase _ = MkQuantity 1.0
-- >>> type instance MkUnit "sr" = Base "sr"
-- >>> instance HasCanonicalBaseUnit "sr" where
-- >>>   type CanonicalBaseUnit "sr" = One
-- >>>   conversionBase _ = MkQuantity 1.0
[u| rad = 1 1
  , sr  = 1 1
  |]

-- | Non-SI units accepted for use with them
-- http://www.bipm.org/en/publications/si-brochure/chapter4.html
-- >>> type instance MkUnit "min" = Base "min"
-- >>> instance HasCanonicalBaseUnit "min" where
-- >>>   type CanonicalBaseUnit "min" = MkUnit "s"
-- >>>   conversionBase _ = MkQuantity 1.6666666666666666e-2
-- >>> type instance MkUnit "h" = Base "h"
-- >>> instance HasCanonicalBaseUnit "h" where
-- >>>   type CanonicalBaseUnit "h" = MkUnit "s"
-- >>>   conversionBase _ = MkQuantity 2.777777777777778e-4
-- >>> type instance MkUnit "d" = Base "d"
-- >>> instance HasCanonicalBaseUnit "d" where
-- >>>   type CanonicalBaseUnit "d" = MkUnit "s"
-- >>>   conversionBase _ = MkQuantity 1.1574074074074073e-5
-- >>> type instance MkUnit "ha" = Base "ha"
-- >>> instance HasCanonicalBaseUnit "ha" where
-- >>>   type CanonicalBaseUnit "ha" = (^:) (MkUnit "m") 2
-- >>>   conversionBase _ = MkQuantity 1.0e-4
-- >>> type instance MkUnit "l" = Base "l"
-- >>> instance HasCanonicalBaseUnit "l" where
-- >>>   type CanonicalBaseUnit "l" = (^:) (MkUnit "m") 3
-- >>>   conversionBase _ = MkQuantity 1000.0
-- >>> type instance MkUnit "t" = Base "t"
-- >>> instance HasCanonicalBaseUnit "t" where
-- >>>   type CanonicalBaseUnit "t" = MkUnit "kg"
-- >>>   conversionBase _ = MkQuantity 1.0e-3
-- >>> type instance MkUnit "au" = Base "au"
-- >>> instance HasCanonicalBaseUnit "au" where
-- >>>   type CanonicalBaseUnit "au" = MkUnit "m"
-- >>>   conversionBase _ = MkQuantity 6.684587122268445e-12
[u| min = 60 s
  , h = 3600 s
  , d = 86400 s
  , ha = 10000 m^2
  , l = 0.001 m^3
  , t = 1000 kg
  , au = 149597870700 m |]

-- | Some random other units
-- >>> type instance MkUnit "ft" = Base "ft"
-- >>> instance HasCanonicalBaseUnit "ft" where
-- >>>   type CanonicalBaseUnit "ft" = MkUnit "m"
-- >>>   conversionBase _ = MkQuantity 3.28
-- >>> type instance MkUnit "in" = Base "in"
-- >>> instance HasCanonicalBaseUnit "in" where
-- >>>   type CanonicalBaseUnit "in" = MkUnit "m"
-- >>>   conversionBase _ = MkQuantity 39.37007874015748
-- >>> type instance MkUnit "mi" = Base "mi"
-- >>> instance HasCanonicalBaseUnit "mi" where
-- >>>   type CanonicalBaseUnit "mi" = MkUnit "m" conversionBase _ = MkQuantity 6.213711922373339e-4
-- >>> type instance MkUnit "mph" = (/:) (MkUnit "mi") (MkUnit "h")
[u| ft = 100 % 328 m, in = 0.0254 m, mi = 1609.344 m, mph = mi/h |]
