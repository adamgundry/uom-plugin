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
    ( -- * Singletons for integers
      STypeInt(..)
    , forgetSTypeInt
    , KnownTypeInt(..)
    , typeIntVal

      -- * Singletons for units
    , SUnit(..)
    , forgetSUnit
    , KnownUnit(..)
    , unitVal
    ) where

import GHC.TypeLits

import Data.UnitsOfMeasure.Internal


-- | Singleton type for type-level integers, with redundant zeroes
data STypeInt (i :: TypeInt) where
  SPos :: KnownNat n => proxy n -> STypeInt (Pos n)
  SNeg :: KnownNat n => proxy n -> STypeInt (Neg n)

-- | Extract the runtime integer from a singleton type-level integer
forgetSTypeInt :: STypeInt i -> Integer
forgetSTypeInt (SPos p) = natVal p
forgetSTypeInt (SNeg p) = - (natVal p)


-- | A constraint @'KnownTypeInt' i@ means that @i@ must be a
-- type-level integer that is statically known but passed at runtime
class KnownTypeInt (i :: TypeInt) where
  typeIntSing :: STypeInt i

instance KnownNat n => KnownTypeInt (Pos n) where
  typeIntSing  = SPos (undefined :: proxy n)

instance KnownNat n => KnownTypeInt (Neg n) where
  typeIntSing  = SNeg (undefined :: proxy n)

-- | Extract the runtime integer from a 'KnownTypeInt'
typeIntVal :: forall proxy i . KnownTypeInt i => proxy i -> Integer
typeIntVal _ = forgetSTypeInt (typeIntSing :: STypeInt i)


-- | Singleton type for concrete units of measure represented as lists
-- of (base unit, exponent) pairs
data SUnit (u :: [(Symbol, TypeInt)]) where
  SNil  :: SUnit '[]
  SCons :: KnownSymbol b => proxy b -> STypeInt i -> SUnit xs -> SUnit ('(b, i) ': xs)

-- | Extract the runtime (base unit, exponent) list from a singleton unit
forgetSUnit :: SUnit u -> [(String, Integer)]
forgetSUnit SNil           = []
forgetSUnit (SCons pb i x) = (symbolVal pb, forgetSTypeInt i) : forgetSUnit x


-- | A constraint @'KnownUnit' u@ means that @u@ must be a concrete
-- unit that is statically known but passed at runtime
class KnownUnit (u :: [(Symbol, TypeInt)]) where
  unitSing :: SUnit u

instance KnownUnit '[] where
  unitSing = SNil

instance (KnownSymbol b, KnownTypeInt i, KnownUnit xs) => KnownUnit ('(b, i) ': xs) where
  unitSing = SCons (undefined :: proxy b) typeIntSing unitSing

-- | Extract the runtime (base unit, exponent) list from a 'KnownUnit'
unitVal :: forall proxy u . KnownUnit u => proxy u -> [(String, Integer)]
unitVal _ = forgetSUnit (unitSing :: SUnit u)
