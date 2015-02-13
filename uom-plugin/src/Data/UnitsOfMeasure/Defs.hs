{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.UnitsOfMeasure.Defs
    (
    ) where

import Data.UnitsOfMeasure

-- The SI base units
-- http://www.bipm.org/en/measurement-units/
[u| m, kg, s, A, K, mol, cd |]

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
[u| ft, in |]
