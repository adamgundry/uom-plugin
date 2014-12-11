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


module Data.UnitsOfMeasure.Show
    ( showQuantity
    , showUnit
    ) where

import Data.UnitsOfMeasure
import Data.UnitsOfMeasure.Singleton

-- | Render a quantity nicely, followed by its units
showQuantity :: forall a u. (Show a, KnownUnit (Unpack u)) => Quantity a u -> String
showQuantity x = show (unQuantity x) ++ " " ++ showUnit (undefined :: proxy u)

-- | Render a unit nicely
showUnit :: forall proxy u . KnownUnit (Unpack u) => proxy u -> String
showUnit _ = showUnitBits (getUnit (undefined :: proxy' (Unpack u)))

showUnitBits :: [(String, Integer)] -> String
showUnitBits []     = "1"
showUnitBits [x]    = showAtom x
showUnitBits (x:xs) = showAtom x ++ " " ++ showUnitBits xs

showAtom :: (String, Integer) -> String
showAtom (s, 1) = s
showAtom (s, i) = s ++ "^" ++ show i
