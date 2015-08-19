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

-- | This module defines singleton types for integers and concrete
-- units.
module Data.UnitsOfMeasure.Singleton
    ( -- * Singletons for units
      SUnit(..)
    , forgetSUnit
    , KnownUnit(..)
    , unitVal

    , SList(..)
    ) where

import GHC.TypeLits

import Data.UnitsOfMeasure.Internal


-- | Singleton type for concrete units of measure represented as lists
-- of (base unit, exponent) pairs
data SUnit (u :: UnitSyntax) where
  SUnit :: SList xs -> SList ys -> SUnit (xs :/ ys)

data SList (xs :: [Symbol]) where
  SNil :: SList '[]
  SCons :: KnownSymbol x => proxy x -> SList xs -> SList (x ': xs)

-- | Extract the runtime (base unit, exponent) list from a singleton unit
forgetSUnit :: SUnit u -> ([String], [String])
forgetSUnit (SUnit xs ys) = (forgetSList xs, forgetSList ys)

forgetSList :: SList xs -> [String]
forgetSList SNil = []
forgetSList (SCons px xs) = symbolVal px : forgetSList xs


-- | A constraint @'KnownUnit' u@ means that @u@ must be a concrete
-- unit that is statically known but passed at runtime
class KnownUnit (u :: UnitSyntax) where
  unitSing :: SUnit u

instance (KnownList xs, KnownList ys) => KnownUnit (xs :/ ys) where
  unitSing = SUnit listSing listSing


class KnownList (xs :: [Symbol]) where
  listSing :: SList xs

instance KnownList '[] where
  listSing = SNil

instance (KnownSymbol x, KnownList xs) => KnownList (x ': xs) where
  listSing = SCons (undefined :: proxy x) listSing

-- | Extract the runtime (base unit, exponent) list from a 'KnownUnit'
unitVal :: forall proxy u . KnownUnit u => proxy u -> ([String], [String])
unitVal _ = forgetSUnit (unitSing :: SUnit u)
