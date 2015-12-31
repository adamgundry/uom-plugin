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
    ( MkUnit
    ) where

import Data.UnitsOfMeasure

-- The SI base units
-- http://www.bipm.org/en/measurement-units/
[u| m, kg, s, A, K, mol, cd |]

-- Some prefixed units
[u| km = 1000m, g = 0.001 kg |]

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

-- SI derived units that require explicit conversion
[u| rad = 1 1
  , sr  = 1 1
  |]

-- Non-SI units accepted for use with them
-- http://www.bipm.org/en/publications/si-brochure/chapter4.html
[u| min = 60 s
  , h = 3600 s
  , d = 86400 s
  , ha = 10000 m^2
  , l = 0.001 m^3
  , t = 1000 kg
  , au = 149597870700 m |]

-- Some random other units
[u| ft = 100 % 328 m, in = 0.0254 m, mi = 1609.344 m, mph = mi/h |]
