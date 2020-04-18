{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | This module gives a brief introduction to the @uom-plugin@
-- library.
module Data.UnitsOfMeasure.Tutorial
  (
  -- * Introduction
  -- $intro

  -- * Prerequisites
  -- $prereqs

  -- * Interactive Use
  -- $ghci

  -- * The 'Unit' Kind
  -- $units

  -- * Declaring Units
  -- $decls

  -- * Creating Quantities
  -- $create

  -- ** Literals
  -- $literal

  -- ** Dimensionless Literals
  -- $dimless

  -- ** Multiplication by One
  -- $multiplication-by-one

  -- ** Attaching Units
  -- $attach

  -- ** Detaching Units
  -- $detach

  -- * Operations on Quantities
  -- $ops

  -- * Unit polymorphism
  -- $polymorphism

  -- * Further Reading
  -- $reading
  ) where

import Data.UnitsOfMeasure

-- $setup
-- >>> import Data.UnitsOfMeasure.Defs ()

-- $intro
--
-- The @uom-plugin@ adds support for type safe units of measure.

-- $prereqs
--
-- To use the @uom-plugin@ library, simply import "Data.UnitsOfMeasure"
-- and pass the option @-fplugin Data.UnitsOfMeasure.Plugin@ to GHC, for
-- example by adding the following above the module header of your source
-- files:
--
-- > {-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
--
-- This will enable the typechecker plugin, which automatically solves
-- equality constraints between units of measure.  You will also need
-- some language extensions:
--
-- > {-# LANGUAGE DataKinds, QuasiQuotes, TypeOperators #-}
--
-- In order to declare new units, you will need:
--
-- > {-# LANGUAGE TypeFamilies, UndecidableInstances #-}

-- $ghci
--
-- If experimenting with @uom-plugin@ in GHCi you will need to
-- activate the plugin with the command
--
-- >>> :seti -fplugin Data.UnitsOfMeasure.Plugin
--
-- otherwise you will get mysterious unsolved constraint errors.  You
-- will probably also need the extensions:
--
-- >>> :seti -XDataKinds -XQuasiQuotes -XTypeOperators

-- $units
--
-- Units of measure, such as kilograms or metres per second, are
-- represented by the abstract kind 'Unit'.  They can be built out of
-- 'One', 'Base', ('Data.UnitsOfMeasure.Internal.*:'),
-- ('Data.UnitsOfMeasure.Internal./:') and
-- ('Data.UnitsOfMeasure.Internal.^:').  Base units are represented as
-- type-level strings (with kind 'Symbol').  For example,
--
-- >>> :kind One
-- One :: Unit
--
-- >>> :kind Base "m" /: Base "s"
-- Base "m" /: Base "s" :: Unit
--
-- The TH quasiquoter 'u' is provided to give a nice syntax for units
-- (see @Text.Parse.Units@ from the @units-parser@ package for details
-- of the syntax).  When used in a type, the quasiquoter produces an
-- expression of kind 'Unit', for example
--
-- >>> :kind! [u| m^2 |]
-- [u| m^2 |] :: Unit
-- = Base "m" *: Base "m"
--
-- >>> :kind! [u| kg m/s |]
-- [u| kg m/s |] :: Unit
-- = (Base "kg" *: Base "m") /: Base "s"

-- $decls
--
-- Base and derived units need to be declared before use, otherwise
-- you will get unsolved constraints like @'KnownUnit' ('Unpack' ('MkUnit' "m"))@.
-- When the TH quasiquoter 'u' is used as in a declaration context, it
-- creates new base or derived units.  Alternatively,
-- 'declareBaseUnit' and 'declareDerivedUnit' can be used as top-level
-- TH declaration splices.  For example:
--
-- > declareBaseUnit "m"
-- > declareDerivedUnit "N" "kg m / s^2"
-- > [u| kg, s |]
--
-- Note that these lines must appear in a module, not GHCi.  For
-- experimenting interactively, "Data.UnitsOfMeasure.Defs" provides
-- definitions of common units, but is subject to change.

-- $create
--
-- A numeric value @__a__@ annotated with units @__u__@ is a @'Quantity' __a__
-- __u__@. Without using the internal @'MkQuantity'@ constructor, we can use
-- literals, multiplication by 1 and unit attaching functions to create
-- quantities.

-- $literal
--
-- For literal quantities use the 'u' quasiquoter, putting the number before
-- the unit. The most general polymorphic type will be inferred.
--
-- >>> :type [u| 1 m |]
-- [u| 1 m |] :: Num a => Quantity a (Base "m")
-- >>> :type [u| 1.0 m |]
-- [u| 1.0 m |] :: Fractional a => Quantity a (Base "m")
-- >>> :type [u| 1 % 1 m |]
-- [u| 1 % 1 m |] :: Fractional a => Quantity a (Base "m")
--
-- Adding a full or partial type signature can make the underlying
-- representational type more concrete.
--
-- >>> :seti -XPartialTypeSignatures -fno-warn-partial-type-signatures
--
-- >>> :type [u| 1 m |] :: _ Int _
-- [u| 1 m |] :: _ Int _ :: Quantity Int (Base "m")
-- >>> :type [u| 1 m |] :: _ Double _
-- [u| 1 m |] :: _ Double _ :: Quantity Double (Base "m")
-- >>> :type [u| 1 m |] :: _ Rational _
-- [u| 1 m |] :: _ Rational _ :: Quantity Rational (Base "m")
--
-- Note how the 'u' quasiquoter can be used for the units in the type too. This
-- is redundant repetition with a literal but is useful when adding type
-- signatures elsewhere.
--
-- >>> [u| 1.1 m / s |] :: Quantity Double [u| m / s |]
-- [u| 1.1 m / s |]

-- $dimless
--
-- The same can be done without putting the value in the quote and without
-- templating altogether for dimensionless units, those with units of 'One'.
--
-- >>> :type 1 :: Quantity _ [u| 1 |]
-- 1 :: Quantity _ [u| 1 |] :: Num w => Quantity w One
-- >>> :type 1.0 :: Quantity _ One
-- 1.0 :: Quantity _ One :: Fractional w => Quantity w One
-- >>> :type 1.0 :: Quantity _ One
-- 1.0 :: Quantity _ One :: Fractional w => Quantity w One
--
-- Things get a little weird when not being explicit about Quantity.
--
-- >>> :type 1 :: _ _ [u| 1 |]
-- 1 :: _ _ [u| 1 |] :: Num (w1 w2 One) => w1 w2 One
-- >>> :type 1 :: _ _ One
-- 1 :: _ _ One :: Num (w1 w2 One) => w1 w2 One
-- >>> :type 1.0 :: _ _ One
-- 1.0 :: _ _ One :: Fractional (w1 w2 One) => w1 w2 One
--
-- >>> 2 :: Quantity Int One
-- [u| 2 |]
-- >>> [u| 2 |]
-- [u| 2 |]
-- >>> :type [u| 2 |]
-- [u| 2 |] :: Num a => Quantity a One

-- $multiplication-by-one
--
-- The product of a numeric value and one of a unit will attach those units to
-- the value.
--
-- >>> [u| 1 m s^-2 |] *: 9.8
-- [u| 9.8 m / s^2 |]
-- >>> 9.8 *: [u| 1 m s^-2 |]
-- [u| 9.8 m / s^2 |]
--
-- The same trick works with dimensionless values.
--
-- >>> [u| 1 m s^-2 |] *: [u| 9.8 |]
-- [u| 9.8 m / s^2 |]
-- >>> [u| 9.8 |] *: [u| 1 m s^-2 |]
-- [u| 9.8 m / s^2 |]

-- $attach
--
-- Quoted units without a value specialise to functions we can use to attach
-- units to unitless numeric values.
--
-- >>> [u| m / s^2 |] 9.8
-- [u| 9.8 m / s^2 |]
-- >>> [u| m s^-2 |] 9.8
-- [u| 9.8 m / s^2 |]
-- >>> [u| m / s / s |] 9.8
-- [u| 9.8 m / s^2 |]
-- >>> [u| s^-2 m |] 9.8
-- [u| 9.8 m / s^2 |]
--
-- Composition of these functions doesn't work as expected. It doesn't apply
-- composed units to the numeric value.
--
-- >>> [u| m |] $ [u| s^-2 |] 9.8
-- [u| [u| 9.8 s^-2 |] m |]
-- >>> [u| s^-2 |] $ [u| m |] 9.8
-- [u| [u| 9.8 m |] s^-2 |]
-- >>> [u| m |] . [u| s^-2 |] $ 9.8
-- [u| [u| 9.8 s^-2 |] m |]
-- >>> [u| s^-2 |] . [u| m |] $ 9.8
-- [u| [u| 9.8 m |] s^-2 |]
-- >>> [u| m |] [u| 9.8 s^-2 |]
-- [u| [u| 9.8 s^-2 |] m |]
-- >>> [u| s^-2 |] [u| 9.8 m |]
-- [u| [u| 9.8 m |] s^-2 |]
-- >>> [u| m |] . [u| s^-1 |] . [u| s^-1 |] $ 9.8
-- [u| [u| [u| 9.8 s^-1 |] s^-1 |] m |]

-- $detach
--
-- The underlying numeric value of a quantity may be extracted with
-- 'unQuantity', detaching the units:
--
-- >>> unQuantity [u| 15 kg |]
-- 15
-- >>> unQuantity [u| 9.8 m / s^2 |]
-- 9.8
-- >>> unQuantity <$> [[u| 1 kg |], [u| 2 kg |]]
-- [1,2]
-- >>> unQuantity . [u| kg |] <$> [1,2]
-- [1,2]
-- >>> :type unQuantity . [u| kg |]
-- unQuantity . [u| kg |] :: c -> c

-- $ops
--
-- The usual arithmetic operators from 'Num' and related typeclasses
-- are restricted to operating on dimensionless quantities.  Thus
-- using them directly on quantities with units will result in errors:
--
-- >>> 2 * [u| 5 m |]
-- <BLANKLINE>
-- ...
-- ... Couldn't match type ‘Base "m"’ with ‘One’
-- ...
--
--
-- >>> [u| 2 m/s |] + [u| 5 m/s |]
-- <BLANKLINE>
-- ...
-- ... Couldn't match type ‘Base "m" /: Base "s"’ with ‘One’
-- ...
--
-- Instead, "Data.UnitsOfMeasure" provides more general arithmetic
-- operators including ('+:'), ('-:'), ('*:') and ('/:').  These may
-- be used to perform unit-safe arithmetic:
--
-- >>> 2 *: [u| 5 m |]
-- [u| 10 m |]
--
-- >>> [u| 2 m / s |] +: [u| 5 m / s |]
-- [u| 7 m / s |]
--
-- However, unit errors will be detected by the type system:
--
-- >>>  [u| 3 m |] -: [u| 1 s |]
-- <BLANKLINE>
-- ...
-- ... Couldn't match type ‘Base "m"’ with ‘Base "s"’
-- ...

-- $polymorphism
--
-- It is easy to work with arbitrary units (type variables of kind
-- 'Unit') rather than particular choices of unit.  The typechecker
-- plugin ensures that type inference is well-behaved and
-- automatically solves equations between units (e.g. making unit
-- multiplication commutative):
--
-- >>> let cube x = x *: x *: x
-- >>> :t cube
-- cube :: Num a => Quantity a v -> Quantity a (v *: (v *: v))
--
-- >>> let f x y = (x *: y) +: (y *: x)
-- >>> :t f
-- f :: Num a => Quantity a v -> Quantity a u -> Quantity a (u *: v)


-- $reading
--
--  * <http://adam.gundry.co.uk/pub/typechecker-plugins/ Paper about uom-plugin>
--
--  * <https://ghc.haskell.org/trac/ghc/wiki/Plugins/TypeChecker Plugins on the GHC wiki>
