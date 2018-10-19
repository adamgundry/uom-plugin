{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Flight.Units.DegMinSec
    ( DMS(..)
    , toDeg
    , toQDeg
    , toQRad
    , fromQ
    ) where

import Prelude hiding (min)
import Data.Fixed (mod')
import Data.Text.Lazy (unpack)
import Formatting (format)
import qualified Formatting.ShortFormatters as Fmt (sf)
import Data.UnitsOfMeasure ((+:), u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.UnitsOfMeasure.Convert (Convertible)

import Flight.Units.Angle (Angle(..))

newtype DMS = DMS (Int, Int, Double) deriving Eq

instance Show DMS where
    show = showDMS

secToShow :: Double -> String
secToShow sec =
    if fromIntegral isec == sec
        then show (abs isec)
        else unpack $ format Fmt.sf (abs sec)
    where
        isec :: Int
        isec = floor sec

showDMS :: DMS -> String
showDMS (DMS (deg, 0, 0)) =
    show deg ++ "°"
showDMS (DMS (0, 0, sec)) =
    secToShow sec ++ "''"
showDMS dms@(DMS (deg, min, 0)) =
    signSymbolDMS dms
    ++ show (abs deg)
    ++ "°"
    ++ show (abs min)
    ++ "'"
showDMS dms@(DMS (0, min, sec)) =
    signSymbolDMS dms
    ++ show (abs min)
    ++ "'"
    ++ secToShow sec
    ++ "''"
showDMS dms@(DMS (deg, min, sec)) =
    signSymbolDMS dms
    ++ show (abs deg)
    ++ "°"
    ++ show (abs min)
    ++ "'"
    ++ secToShow sec
    ++ "''"

toDeg :: DMS -> Double
toDeg dms@(DMS (deg, min, s)) =
    signDMS dms * (abs d + abs m / 60 + abs s / 3600)
    where
        d = fromIntegral deg
        m = fromIntegral min

signSymbolDMS :: DMS -> String
signSymbolDMS dms =
    if signDMS dms < 0 then "-" else ""

signDMS :: DMS -> Double
signDMS (DMS (deg, min, s)) =
    if elem (-1) $ signum <$> [d, m, s] then -1 else 1
    where
        d = fromIntegral deg
        m = fromIntegral min 

toQDeg :: DMS -> Quantity Double [u| deg |]
toQDeg =
    MkQuantity . toDeg

toQRad :: DMS -> Quantity Double [u| rad |]
toQRad =
    convert . toQDeg

fromQ :: Convertible u [u| deg |] => Quantity Double u -> DMS
fromQ q' =
    DMS (s * dd, mm, ss)
    where
        MkQuantity d = convert q' :: Quantity Double [u| deg |]

        totalSecs :: Int
        totalSecs = round $ 3600.0 * d

        s = signum totalSecs

        (dd, ms) = quotRem (abs totalSecs) 3600
        mm = quot ms 60

        ss =
            (abs d - fromIntegral dd) * 3600.0
            - fromIntegral (mm * 60)

instance Angle DMS where
    normalize dms =
        fromQuantity n
        where
            n :: Quantity Double [u| deg |]
            n = MkQuantity $ d `mod'` 360.0

            (MkQuantity d) = toQuantity dms :: Quantity Double [u| deg |]

    rotate rotation dms =
        normalize . fromQuantity $ d +: r
        where
            r :: Quantity Double [u| deg |]
            r = toQuantity rotation

            d :: Quantity Double [u| deg |]
            d = toQuantity dms

    fromQuantity = fromQ
    toQuantity = convert . toQDeg

instance Ord DMS where
    x <= y = x' <= y'
        where
            x' :: Quantity Double [u| deg |]
            x' = toQuantity $ normalize x

            y' :: Quantity Double [u| deg |]
            y' = toQuantity $ normalize y
