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

module Data.UnitsOfMeasure.Singleton
    ( STypeInt(..)
    , KnownTypeInt(..)
    , power
    , SUnit(..)
    , KnownUnit(..)
    ) where

import GHC.TypeLits

import Data.UnitsOfMeasure
import Data.UnitsOfMeasure.Internal


data STypeInt (i :: TypeInt) where
  SPos :: KnownNat n => proxy n -> STypeInt (Pos n)
  SNeg :: KnownNat n => proxy n -> STypeInt (Neg n)

class KnownTypeInt (i :: TypeInt) where
  typeIntVal  :: proxy i -> Integer
  typeIntSing :: STypeInt i

instance KnownNat n => KnownTypeInt (Pos n) where
  typeIntVal _ = natVal (undefined :: proxy n)
  typeIntSing  = SPos (undefined :: proxy n)

instance KnownNat n => KnownTypeInt (Neg n) where
  typeIntVal _ = - (natVal (undefined :: proxy n))
  typeIntSing  = SNeg (undefined :: proxy n)


power :: Fractional a => Quantity a u -> STypeInt i -> Quantity a (u ^^: i)
power (MkQuantity x) (SPos p) = MkQuantity (x ^^ natVal p)
power (MkQuantity x) (SNeg p) = MkQuantity (x ^^ (- natVal p))


data SUnit (u :: [(Symbol, TypeInt)]) where
  SNil  :: SUnit '[]
  SCons :: proxy b -> STypeInt i -> SUnit xs -> SUnit ('(b, i) ': xs)

class KnownUnit (u :: [(Symbol, TypeInt)]) where
  getUnit  :: proxy u -> [(String, Integer)]
  unitSing :: SUnit u

instance KnownUnit '[] where
  getUnit _ = []
  unitSing = SNil

instance (KnownSymbol b, KnownTypeInt i, KnownUnit xs) => KnownUnit ('(b, i) ': xs) where
  getUnit _ = ( symbolVal (undefined :: proxy b)
              , typeIntVal (undefined :: proxy i)
              ) : getUnit (undefined :: proxy xs)
  unitSing = SCons (undefined :: proxy b) typeIntSing unitSing
