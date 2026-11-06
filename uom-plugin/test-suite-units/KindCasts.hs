{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -dcore-lint #-}

-- Tests for looking through casts in units.
module KindCasts
  ( castWanted
  , castRigid
  , castGiven
  , castFamily
  , castKindEquality
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))

import Data.UnitsOfMeasure (Unit, Base, type (*:), type (/:))

type family GradeKind (g :: Type) :: Type
type instance GradeKind () = Unit

root :: forall (i :: GradeKind ()). Proxy (i *: i) -> Proxy (i *: Base "s")
root _ = Proxy

-- i *: i ~ m *: m  =>  i ~ m
castWanted :: Proxy (Base "m" *: Base "s")
castWanted = root (Proxy :: Proxy (Base "m" *: Base "m"))

mul :: forall (i :: GradeKind ()) (j :: GradeKind ()). Proxy (i *: j) -> Proxy j -> ()
mul _ _ = ()

-- i *: v ~ m  =>  i ~ m /: v, which mentions the cast unit v
castRigid :: forall (v :: GradeKind ()). Proxy v -> ()
castRigid = mul (Proxy :: Proxy (Base "m"))

-- given i *: i ~ m *: m  =>  given i ~ m
castGiven :: forall (i :: GradeKind ()).
             (i *: i) :~: (Base "m" *: Base "m")
          -> Proxy (i *: Base "s") -> Proxy (Base "m" *: Base "s")
castGiven Refl p = p

type family Stuck (a :: Type) :: GradeKind ()

-- i *: i ~ Stuck a *: Stuck a  =>  i ~ Stuck a
castFamily :: forall a. Proxy (Stuck a *: Base "s")
castFamily = root (Proxy :: Proxy (Stuck a *: Stuck a))

-- a *: b ~ m  =>  a ~ m /: b
castKindEquality
  :: forall (a :: GradeKind ()) (b :: GradeKind ()). ((a *: b) ~ Base "m")
  => Proxy a -> Proxy (Base "m" /: b)
castKindEquality = id
