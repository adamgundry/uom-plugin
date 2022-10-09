{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Very very rough support for reading units of measure in the
-- syntax used by "Data.UnitsOfMeasure.Show".
module Data.UnitsOfMeasure.Read
   ( readQuantity
   , readUnit
   , readWithUnit
   , readWithUnit'
   , QuantityWithUnit(..)
   ) where

import Control.Monad (join)
import Data.List (genericReplicate)
import Data.Type.Equality ((:~:)(..))
import Text.Parse.Units (parseUnit, universalSymbolTable, UnitExp(..))

import Data.UnitsOfMeasure.BaseUnitMap
import Data.UnitsOfMeasure.Internal
import Data.UnitsOfMeasure.Singleton

-- | Represents a quantity whose units have a syntactic representation
-- that is known statically and at runtime.
data QuantityWithUnit a u where
  QuantityWithUnit :: Quantity a (Pack u) -> SUnit u -> QuantityWithUnit a u


instance (KnownUnit (Unpack u), u ~ Pack (Unpack u), Read a) => Read (Quantity a u) where
  readsPrec i (' ':s) = readsPrec i s
  readsPrec _ ('[':'u':'|':s)
   | (t, '|':']':r) <- break (== '|') s
   , Right v <- readWithUnit @u t = [(v, r)]
  readsPrec _ _ = []


-- | Parse a quantity and check that it has the expected units.  Only base units
-- mentioned in the expected unit may be mentioned; other base units will lead
-- to a parse failure.
readWithUnit :: forall u a . (KnownUnit (Unpack u), u ~ Pack (Unpack u), Read a)
             => String -> Either String (Quantity a u)
readWithUnit = readWithUnit' (unitToBaseUnitMap (unitSing @(Unpack u)))

-- | Parse a quantity and check that it has the expected units.  This variant
-- allows the available base units to be specified.  The base units in the map
-- must be a superset of 'unitToBaseUnitMap' for the expected units.
readWithUnit' :: forall u a . (KnownUnit (Unpack u), u ~ Pack (Unpack u), Read a)
             => BaseUnitMap -> String -> Either String (Quantity a u)
readWithUnit' base_units s = do
    Some (QuantityWithUnit (q :: Quantity a _) v) <- readQuantity base_units s
    case testEquivalentSUnit u v of
        Just Refl -> Right q
        Nothing   -> Left ("wrong units: expected " ++ show (forgetSUnit u)
                            ++ " but got " ++ show (forgetSUnit v))
  where
    u = unitSing @(Unpack u)

-- | Parse a quantity along with its units.
readQuantity :: Read a => BaseUnitMap -> String -> Either String (Some (QuantityWithUnit a))
readQuantity base_units s = case reads s of
    [(n, s')] -> do Some u <- readUnit base_units s'
                    return $ Some (QuantityWithUnit (MkQuantity n) u)
    _         -> Left "reads: no parse"

-- | Parse a unit.
readUnit :: BaseUnitMap -> String -> Either String (Some SUnit)
readUnit base_units s = expToSomeUnit base_units =<< parseUnit universalSymbolTable s

expToSomeUnit :: BaseUnitMap -> UnitExp () String -> Either String (Some SUnit)
expToSomeUnit base_units = unitSyntaxToSomeUnit base_units . expToUnitSyntax

unitSyntaxToSomeUnit :: BaseUnitMap -> UnitSyntax String -> Either String (Some SUnit)
unitSyntaxToSomeUnit base_units (xs :/ ys) = do
    Some xs' <- someListVal base_units xs
    Some ys' <- someListVal base_units ys
    pure $ Some (SUnit xs' ys')

someListVal :: BaseUnitMap -> [String] -> Either String (Some SList)
someListVal _          []     = pure $ Some SNil
someListVal base_units (x:xs) = do
    Some x'@SBaseUnit{} <- maybe (Left ("unknown unit: " ++ x)) pure $ lookupBaseUnitMap x base_units
    Some xs' <- someListVal base_units xs
    pure $ Some (SCons x' xs')

expToUnitSyntax :: UnitExp () String -> UnitSyntax String
expToUnitSyntax Unity = [] :/ []
expToUnitSyntax (Unit _ s) = [s] :/ []
expToUnitSyntax (u `Mult` v) = (u_xs ++ v_xs) :/ (u_ys ++ v_ys)
  where
    u_xs :/ u_ys = expToUnitSyntax u
    v_xs :/ v_ys = expToUnitSyntax v
expToUnitSyntax (u `Div` v) = (u_xs ++ v_ys) :/ (u_ys ++ v_xs)
  where
    u_xs :/ u_ys = expToUnitSyntax u
    v_xs :/ v_ys = expToUnitSyntax v
expToUnitSyntax (u `Pow` n)
    | n >= 0    = join (genericReplicate n u_xs) :/ join (genericReplicate n u_ys)
    | otherwise = join (genericReplicate (negate n) u_ys) :/ join (genericReplicate (negate n) u_xs)
  where
    u_xs :/ u_ys = expToUnitSyntax u
