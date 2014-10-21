{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -ftc-plugin Data.UnitsOfMeasure.Plugin #-}
module Working where

import Data.UnitsOfMeasure
import Data.UnitsOfMeasure.TH

import GHC.Prim (Proxy#, proxy#)
import GHC.TypeLits

type instance MkUnit "ft" = Base "ft"
type instance MkUnit "kg" = Base "kg"
type instance MkUnit "m"  = Base "m"
type instance MkUnit "s"  = Base "s"
type instance MkUnit "N"  = [u|kg*m/s^2|]

gravityOnEarth :: Quantity Double [u|m/s^2|]
gravityOnEarth = 9.808 % [u|m/s^2|]

heightOfBuilding :: a ~ [u|m|] => Quantity Double a
heightOfBuilding = 40.0 % [u|m|]

speedOfImpact = sqrt' (2.0 *: (gravityOnEarth *: heightOfBuilding))

myMass :: Fractional a => Quantity a [u|kg|]
myMass = 65.0 % [u|kg|]

forceOnGround :: Quantity Double [u|N*s^0|]
forceOnGround = myMass *: gravityOnEarth

attract (m1 :: Quantity a [u|kg|]) (m2 :: Quantity a [u|kg|]) (r :: Quantity a [u|m|])
    = _G *: m1 *: m2 /: (r *: r) :: Quantity a [u|N|]
  where
    _G = 6.67384e-11 % (proxy# :: Proxy# ([u|N*m^2|] /: [u|kg^2|]))

aTime :: Num a => Quantity a [u|s|]
aTime = 10 % [u|s|]

feetPerMetre = 3.28084 `unit` [u|ft/m|]

speedOfImpactInFPS :: Quantity Double [u|ft/s|]
speedOfImpactInFPS = speedOfImpact *: feetPerMetre

sum' xs = foldr (+:) zero xs

mean xs = sum' xs /: float (length xs)
  where
    float :: Fractional a => Int -> Quantity a One
    float = mk . fromRational . fromInteger . toInteger


diff h f = \ x -> (f (x+:h) -: f(x-:h)) /: (2.0*:h)

earthMass = 5.9736e24 % [u|kg|]


it :: Quantity Double [u|m|] -> Quantity Double [u|N/m|]
it = diff (0.01 % [u|m|]) (attract myMass earthMass)


data Complex u = Complex  { re :: Quantity Double u
                          , im :: Quantity Double u }
data Vector3 u = Vector3  { vx :: Quantity Double u
                          , vy :: Quantity Double u
                          , vz :: Quantity Double u
                          }
  deriving (Eq, Show)
data Sphere  u = MkSphere { centre :: Vector3 u, radius :: Quantity Double u }
data Obj     u = Sph (Sphere u) | Group [Obj u]

gravity = Vector3 { vx = zero, vy = zero, vz = negate' gravityOnEarth }

dot v w = vx v *: vx w +: vy v *: vy w +: vz v *: vz w



data Derivs u v = Cons (Quantity Double u -> Quantity Double v) (Derivs u (v /: u))

makeDerivs :: Quantity Double u -> (Quantity Double u -> Quantity Double v) -> Derivs u v
makeDerivs h f = Cons f $ makeDerivs h (diff h f)

evalDerivs :: Derivs u v -> Quantity Double u -> [Double]
evalDerivs (Cons f ds) x = unQuantity (f x) : evalDerivs ds x



type family (us :: [Unit]) // (v :: Unit) :: [Unit] where
  '[]       // v = '[]
  (u ': us) // v = (u /: v) ': (us // v)

data Vec :: [Unit] -> * -> * where
  VNil  :: Vec '[] a
  VCons :: Quantity a u -> Vec us a -> Vec (u ': us) a

data Matrix :: [Unit] -> [Unit] -> * -> * where
  MNil  :: Matrix us '[] a
  MCons :: Vec (us // v) a -> Matrix us vs a -> Matrix us (v ': vs) a

v1 = VCons (1 % [u|kg/m|]) $ VCons (3 % [u|s/m|]) VNil
v2 = VCons (2 % [u|kg/s|]) $ VCons (4 % [u|s/s|]) VNil

m1 :: Matrix '[ [u|kg|], [u|s|] ] '[ [u|m|], [u|s|] ] Integer
m1 = MCons v1 $ MCons v2 MNil



class Dimension (u :: Unit) (d :: Unit)

instance Dimension (Base "m") (Base "L")
instance Dimension (Base "s") (Base "T")

velocity :: (Fractional a, Dimension u (Base "L"), Dimension v (Base "T")) =>
              Quantity a u -> Quantity a v -> Quantity a (u /: v)
velocity l t = l /: t
