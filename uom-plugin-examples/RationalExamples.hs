{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module RationalExamples (dump) where

import Data.UnitsOfMeasure
import Data.UnitsOfMeasure.Show ()
import Data.UnitsOfMeasure.Defs ()
import Data.UnitsOfMeasure.Internal (Quantity(..), toRational')

-- A nautical mile.
[u| NM = 1852 m |]

-- The radius of Earth as a sphere.
radiusOfEarth :: Quantity Double [u| km |]
radiusOfEarth = convert [u| 6371000 m |]

-- The circumference of Earth.
circumferenceOfEarth :: Quantity Double [u| km |]
circumferenceOfEarth = 2 * pi *: radiusOfEarth

-- A degree of longitude at the equator as a distance.
lngDegree :: Quantity Double [u| km |]
lngDegree = circumferenceOfEarth /: 360

lngDegreeInNM:: Quantity Double [u| NM |]
lngDegreeInNM = convert lngDegree

-- A minute of longitude at the equator as a distance.
lngMinute :: Quantity Double [u| km |]
lngMinute = lngDegree /: 60

lngMinuteInNM:: Quantity Double [u| NM |]
lngMinuteInNM = convert lngMinute

lngMinuteInNMRounded:: Quantity Integer [u| NM |]
lngMinuteInNMRounded = MkQuantity . round . unQuantity $ lngMinuteInNM

lngMinuteInNMCareless :: Quantity Integer [u| NM |]
lngMinuteInNMCareless = MkQuantity . round . unQuantity $ lngMinute

lngMinuteInNMRational :: Quantity Rational [u| NM |]
lngMinuteInNMRational = convert $ toRational' lngMinute

-- A second of longitude at the equator as a distance.
lngSecond :: Quantity Double [u| m |]
lngSecond =
    let k = 1 / 60 :: Quantity Double [u| 1 |]
        s = k *: lngMinute
    in convert s

lngSecondInFeet:: Quantity Double [u| ft |]
lngSecondInFeet = convert lngSecond

dump :: IO ()
dump = do
  putStrLn $ "Earth radius: " ++ show radiusOfEarth
  putStrLn $ "Earth circumference: " ++ show circumferenceOfEarth
  putStrLn "Anywhere, a _ of latitude and at the equator, a _ of longitude ..."
  putStrLn $ "  degree : " ++ show lngDegree
  putStrLn $ "  degree : " ++ show lngDegreeInNM ++ ", NM = nautical mile"
  putStrLn ""
  putStrLn $ "  minute : " ++ show lngMinute
  putStrLn $ "  minute : " ++ show lngMinuteInNM
  putStrLn $ "  minute : " ++ show lngMinuteInNMRounded ++ ", NM rounded to NM"
  putStrLn $ "  minute : " ++ show lngMinuteInNMCareless ++ ", km rounded carelessly as if NM, there's no type-safety with MkQuantity . round . unQuantity"
  putStrLn $ "  minute : " ++ show lngMinuteInNMRational ++ ", Double NM -> Rational NM"
  putStrLn ""
  putStrLn $ "  second : " ++ show lngSecond
  putStrLn $ "  second : " ++ show lngSecondInFeet
