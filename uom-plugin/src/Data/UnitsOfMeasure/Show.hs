{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Experimental support for showing units of measure in a pretty
-- syntax.  This requires the units to be fully determined.
--
-- Apart from the definitions below, this module also exports a 'Show'
-- instance for @'Quantity' a u@, which is re-exported by
-- "Data.UnitsOfMeasure".
module Data.UnitsOfMeasure.Show
    ( showQuantity
    , showUnit
    ) where

import Data.UnitsOfMeasure.Internal
import Data.UnitsOfMeasure.Singleton

import Data.List (partition)

instance (Show a, KnownUnit (Unpack u)) => Show (Quantity a u) where
  show x = "[u| " ++ showQuantity x ++ " |]"

-- | Render a quantity nicely, followed by its units:
--
-- >>> showQuantity (1 /: [u| 0.1 s / m kg |])
-- "10.0 kg m / s"
showQuantity :: forall a u. (Show a, KnownUnit (Unpack u)) => Quantity a u -> String
showQuantity (MkQuantity x) = show x ++ if s == "1" then "" else ' ':s
  where s = showUnit (undefined :: proxy u)

-- | Render a unit nicely:
--
-- >>> showUnit (undefined :: proxy [u| 1 / s |])
-- "s^-1"
showUnit :: forall proxy u . KnownUnit (Unpack u) => proxy u -> String
showUnit _ = showUnitBits (unitVal (undefined :: proxy' (Unpack u)))

showUnitBits :: [(String, Integer)] -> String
showUnitBits [] = "1"
showUnitBits xs
  | null zs   = showPos ys
  | null ys   = showPos zs
  | otherwise = showPos ys ++ " / " ++ showPos (map (fmap negate) zs)
  where (ys, zs) = partition ((>= 0) . snd) xs

showPos :: [(String, Integer)] -> String
showPos []     = "1"
showPos [x]    = showAtom x
showPos (x:xs) = showAtom x ++ " " ++ showUnitBits xs

showAtom :: (String, Integer) -> String
showAtom (s, 1) = s
showAtom (s, i) = s ++ "^" ++ show i
