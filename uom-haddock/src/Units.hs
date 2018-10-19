{-# OPTIONS_GHC -fno-warn-orphans #-}

module Units where

import Data.UnitsOfMeasure (u)
import Units.Angle ()

[u| s, m |]

-- NOTE: hm is the hectometre, 100m.
[u| km = 1000 m, mm = 1 % 1000 m, hm = 100 m |]

[u| min = 60 s, h = 3600 s, d = 86400 s |]

[u| ft = 100 % 328 m, mi = 1609.344 m, mph = mi/h |]
