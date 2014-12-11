{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Data.UnitsOfMeasure.Convert
    ( HasCanonicalBaseUnit(..)
    , conversionRatio
    , convert
    ) where

import Data.UnitsOfMeasure
import Data.UnitsOfMeasure.Internal
import Data.UnitsOfMeasure.Singleton

import GHC.Exts ( Constraint )
import GHC.TypeLits


class HasCanonicalBaseUnit (b :: Symbol) where
  type CanonicalBaseUnit b :: Symbol
  type CanonicalBaseUnit b = b

  conversionBase :: Fractional a => proxy b -> Quantity a (Base b /: Base (CanonicalBaseUnit b))
  default conversionBase :: (Fractional a, b ~ CanonicalBaseUnit b) => proxy b -> Quantity a (Base b /: Base b)
  conversionBase _ = lem1 (undefined :: proxy (Base b)) 1

type family MapCBU (xs :: [(Symbol, TypeInt)]) :: [(Symbol, TypeInt)] where
  MapCBU '[]             = '[]
  MapCBU ('(b, i) ': xs) = '(CanonicalBaseUnit b, i) ': MapCBU xs

type family HasCanonical (xs :: [(Symbol, TypeInt)]) :: Constraint where
  HasCanonical '[]             = ()
  HasCanonical ('(b, i) ': xs) = (HasCanonicalBaseUnit b, HasCanonical xs)


conversionRatio :: forall proxy a u . ( Fractional a
                                      , KnownUnit (Unpack u)
                                      , HasCanonical (Unpack u)
                                      , u ~ Pack (Unpack u)
                                      )
               => proxy u -> Quantity a (u /: Pack (MapCBU (Unpack u)))
conversionRatio _ = help (unitSing :: SUnit (Unpack u))

help :: forall a xs . (Fractional a, HasCanonical xs) => SUnit xs -> Quantity a (Pack xs /: Pack (MapCBU xs))
help SNil          = 1
help (SCons p i u) = unsafeConvertQuantity $ power (conversionBase p) i *: help u


-- TODO: this is true in the equational theory of units, but
-- solvability is not closed under substitution, because of units like
-- `Base b` where `b` is a variable, which is not currently handled.
lem1 :: proxy u -> Quantity a One -> Quantity a (u /: u)
lem1 _ = id

unsafeConvertQuantity :: Quantity a u -> Quantity a v
unsafeConvertQuantity = MkQuantity . unQuantity


type Good u = (u ~ Pack (Unpack u), KnownUnit (Unpack u), HasCanonical (Unpack u))
type SameDimension u v = Pack (MapCBU (Unpack u)) ~ Pack (MapCBU (Unpack v))

convert :: forall a u v . (Fractional a, Good u, Good v, SameDimension u v)
         => Quantity a u -> Quantity a v
convert = (r *:)
  where
    r = conversionRatio (undefined :: proxy v) /: conversionRatio (undefined :: proxy u)
