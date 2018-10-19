{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Units.Angle (Angle(..)) where

import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.UnitsOfMeasure.Convert (Convertible)

[u| deg |]

class Angle a where
    fromQuantity :: Convertible u [u| deg |] => Quantity Double u -> a
    toQuantity :: Convertible u [u| deg |] => a -> Quantity Double u

instance Convertible u [u| deg |] => Angle (Quantity Double u) where
    fromQuantity = convert
    toQuantity = convert
