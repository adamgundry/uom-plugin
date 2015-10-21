{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

module Data.UnitsOfMeasure.Read
   ( readQuantity
   , readUnit
   , Some(..)
   ) where

import Control.Monad (join)
import Data.List (genericReplicate)
import GHC.TypeLits
import Text.Parse.Units (parseUnit, universalSymbolTable, UnitExp(..))

import Data.UnitsOfMeasure.Internal
import Data.UnitsOfMeasure.Singleton

data SomeQuantity a where
  SomeQuantity :: Quantity a (Pack u) -> SUnit u -> SomeQuantity a

data Some p where
  Some :: p x -> Some p

instance Show (Some SUnit) where
  show (Some u) = show (forgetSUnit u)

readQuantity :: Read a => String -> Either String (SomeQuantity a)
readQuantity s = case reads s of
                   [(n, s')] -> do Some u <- readUnit s'
                                   return $ SomeQuantity (MkQuantity n) u
                   _         -> Left "readQuantity borked"

readUnit :: String -> Either String (Some SUnit)
readUnit s = expToSomeUnit <$> parseUnit universalSymbolTable s

expToSomeUnit :: UnitExp () String -> Some SUnit
expToSomeUnit = unitSyntaxToSomeUnit . expToUnitSyntax

unitSyntaxToSomeUnit :: UnitSyntax String -> Some SUnit
unitSyntaxToSomeUnit (xs :/ ys) = case (someListVal xs, someListVal ys) of
  (Some xs', Some ys') -> Some (SUnit xs' ys')

someListVal :: [String] -> Some SList
someListVal [] = Some SNil
someListVal (x:xs) = case (someSymbolVal x, someListVal xs) of
                       (SomeSymbol x', Some xs') -> Some (SCons x' xs')

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
