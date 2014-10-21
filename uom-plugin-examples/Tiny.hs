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
module Tiny where

import Data.UnitsOfMeasure
import Data.UnitsOfMeasure.TH

-- import GHC.Prim (Proxy#, proxy#)
-- import GHC.TypeLits

type instance MkUnit "ft" = Base "ft"
type instance MkUnit "kg" = Base "kg"
type instance MkUnit "m"  = Base "m"
type instance MkUnit "s"  = Base "s"
type instance MkUnit "N"  = [u|kg*m/s^2|]

gravityOnEarth :: Quantity Double [u|m/s^2|]
gravityOnEarth = 9.808 % [u|m/s s|]
