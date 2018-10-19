{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flight.Units.Angle (Angle(..)) where

import Data.Fixed (mod')
import Data.UnitsOfMeasure ((+:), u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.UnitsOfMeasure.Convert (Convertible)

[u| rad |]
[u| deg = (5030569068109113 % 288230376151711744) rad |]
[u| dms = (5030569068109113 % 288230376151711744) rad |]

class Angle a where
    normalize :: a -> a
    rotate :: a -> a -> a
    fromQuantity :: Convertible u [u| deg |] => Quantity Double u -> a
    toQuantity :: Convertible u [u| deg |] => a -> Quantity Double u

instance Convertible u [u| deg |] => Angle (Quantity Double u) where
    normalize d' =
        convert n
        where
            n :: Quantity Double [u| deg |]
            n = MkQuantity $ d `mod'` 360.0

            (MkQuantity d) = convert d' :: Quantity Double [u| deg |]

    rotate rotation d' =
        normalize . fromQuantity $ d +: r
        where
            r :: Quantity Double [u| deg |]
            r = toQuantity rotation

            d :: Quantity Double [u| deg |]
            d = toQuantity d'

    fromQuantity = convert
    toQuantity = convert
