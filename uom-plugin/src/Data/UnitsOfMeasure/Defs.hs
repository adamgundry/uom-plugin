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
module Data.UnitsOfMeasure.Defs
    (
    ) where

import Data.UnitsOfMeasure
import Data.UnitsOfMeasure.Convert

-- The SI base units
-- http://www.bipm.org/en/measurement-units/
[u| m, kg, s, A, K, mol, cd |]

-- Some prefixed units
[u| km, g |]

-- SI derived units
-- http://physics.nist.gov/cuu/Units/units.html
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

-- Non-SI units accepted for use with them
-- http://www.bipm.org/en/publications/si-brochure/chapter4.html
[u| min, h, d, ha, l, t, au |]

-- Some random other units
[u| ft, in, mi, mph = mi/h |]


instance HasCanonicalBaseUnit "m"

instance HasCanonicalBaseUnit "kg"

instance HasCanonicalBaseUnit "s"


instance HasCanonicalBaseUnit "ft" where
  type CanonicalBaseUnit "ft" = "m"
  conversionBase _ = [u| 3.28 ft/m |]

instance HasCanonicalBaseUnit "in" where
  type CanonicalBaseUnit "in" = "m"
  conversionBase _ = [u| 12 in/ft |] *: ratio [u|ft|] [u|m|]

instance HasCanonicalBaseUnit "km" where
  type CanonicalBaseUnit "km" = "m"
  conversionBase _ = recip' [u| 1000 m/km |]

instance HasCanonicalBaseUnit "mi" where
  type CanonicalBaseUnit "mi" = "m"
  conversionBase _ = recip' [u| 1609.344 m/mi |]


instance HasCanonicalBaseUnit "g" where
  type CanonicalBaseUnit "g" = "kg"
  conversionBase _ = [u| 1000 g/kg |]


instance HasCanonicalBaseUnit "min" where
  type CanonicalBaseUnit "min" = "s"
  conversionBase _ = recip' [u| 60 s/min |]

instance HasCanonicalBaseUnit "h" where
  type CanonicalBaseUnit "h" = "s"
  conversionBase _ = recip' [u| 3600 s/h |]
