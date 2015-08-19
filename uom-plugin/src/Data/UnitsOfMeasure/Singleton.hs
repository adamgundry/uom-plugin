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

      -- * Singletons for lists
    , SList(..)
    , KnownList(..)
    ) where

import GHC.TypeLits

import Data.UnitsOfMeasure.Internal


-- | Singleton type for concrete units of measure represented as lists
-- of base units
data SUnit (u :: UnitSyntax Symbol) where
  SUnit :: SList xs -> SList ys -> SUnit (xs :/ ys)

-- | Singleton type for lists of base units
data SList (xs :: [Symbol]) where
  SNil :: SList '[]
  SCons :: KnownSymbol x => proxy x -> SList xs -> SList (x ': xs)

-- | Extract the runtime syntactic representation from a singleton unit
forgetSUnit :: SUnit u -> UnitSyntax String
forgetSUnit (SUnit xs ys) = forgetSList xs :/ forgetSList ys

forgetSList :: SList xs -> [String]
forgetSList SNil = []
forgetSList (SCons px xs) = symbolVal px : forgetSList xs


-- | A constraint @'KnownUnit' u@ means that @u@ must be a concrete
-- unit that is statically known but passed at runtime
class KnownUnit (u :: UnitSyntax Symbol) where
  unitSing :: SUnit u

instance (KnownList xs, KnownList ys) => KnownUnit (xs :/ ys) where
  unitSing = SUnit listSing listSing


-- | A constraint @'KnownList' xs@ means that @xs@ must be a list of
-- base units that is statically known but passed at runtime
class KnownList (xs :: [Symbol]) where
  listSing :: SList xs

instance KnownList '[] where
  listSing = SNil

instance (KnownSymbol x, KnownList xs) => KnownList (x ': xs) where
  listSing = SCons (undefined :: proxy x) listSing


-- | Extract the runtime syntactic representation of a 'KnownUnit'
unitVal :: forall proxy u . KnownUnit u => proxy u -> UnitSyntax String
unitVal _ = forgetSUnit (unitSing :: SUnit u)
