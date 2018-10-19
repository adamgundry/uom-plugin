{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flight.Units
    ( abs
    , showRadian
    , realToFrac'
    ) where

import Data.UnitsOfMeasure (u, convert, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.Bifunctor.Flip (Flip(..))
import Data.Ratio.Rounding (dpRound)

import Flight.Units.Angle ()

[u| s, m |]

-- NOTE: hm is the hectometre, 100m.
[u| km = 1000 m, mm = 1 % 1000 m, hm = 100 m |]

[u| min = 60 s, h = 3600 s, d = 86400 s |]

[u| ft = 100 % 328 m, mi = 1609.344 m, mph = mi/h |]

-- | Convert any 'Real' quantity into a 'Fractional' type ('realToFrac').
realToFrac' :: (Real a, Fractional b) => Quantity a u -> Quantity b u
realToFrac' (MkQuantity x) = MkQuantity (realToFrac x)

instance Functor (Flip Quantity u) where
    fmap = map'

map' :: (a -> b) -> Flip Quantity u a -> Flip Quantity u b
map' f (Flip (MkQuantity x)) = Flip $ MkQuantity $ f x

showRadian :: Quantity Rational [u| rad |] -> String
showRadian b = show dbl
    where
        deg = convert b :: Quantity Rational [u| deg |]
        Flip rounded = dpRound 3 <$> Flip deg
        MkQuantity dbl = fromRational' rounded :: Quantity Double [u| deg |]
