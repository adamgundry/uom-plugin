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
    , KnownTypeInt(..)
    , typeIntVal
    , forgetSTypeInt

      -- * Singletons for units
    , SUnit(..)
    , KnownUnit(..)
    , unitVal
    , forgetSUnit
    ) where

import GHC.TypeLits

import Data.UnitsOfMeasure.Internal


data STypeInt (i :: TypeInt) where
  SPos :: KnownNat n => proxy n -> STypeInt (Pos n)
  SNeg :: KnownNat n => proxy n -> STypeInt (Neg n)

class KnownTypeInt (i :: TypeInt) where
  typeIntSing :: STypeInt i

instance KnownNat n => KnownTypeInt (Pos n) where
  typeIntSing  = SPos (undefined :: proxy n)

instance KnownNat n => KnownTypeInt (Neg n) where
  typeIntSing  = SNeg (undefined :: proxy n)

typeIntVal :: forall proxy i . KnownTypeInt i => proxy i -> Integer
typeIntVal _ = forgetSTypeInt (typeIntSing :: STypeInt i)

forgetSTypeInt :: STypeInt i -> Integer
forgetSTypeInt (SPos p) = natVal p
forgetSTypeInt (SNeg p) = - (natVal p)


data SUnit (u :: [(Symbol, TypeInt)]) where
  SNil  :: SUnit '[]
  SCons :: KnownSymbol b => proxy b -> STypeInt i -> SUnit xs -> SUnit ('(b, i) ': xs)

class KnownUnit (u :: [(Symbol, TypeInt)]) where
  unitSing :: SUnit u

instance KnownUnit '[] where
  unitSing = SNil

instance (KnownSymbol b, KnownTypeInt i, KnownUnit xs) => KnownUnit ('(b, i) ': xs) where
  unitSing = SCons (undefined :: proxy b) typeIntSing unitSing

unitVal :: forall proxy u . KnownUnit u => proxy u -> [(String, Integer)]
unitVal _ = forgetSUnit (unitSing :: SUnit u)

forgetSUnit :: SUnit u -> [(String, Integer)]
forgetSUnit SNil           = []
forgetSUnit (SCons pb i x) = (symbolVal pb, forgetSTypeInt i) : forgetSUnit x
