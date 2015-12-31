{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Template Haskell utilities for working with units of measure in a
-- nice syntax.
module Data.UnitsOfMeasure.TH
    ( u
    , declareBaseUnit
    , declareDerivedUnit
    , declareConvertibleUnit
    ) where

import Data.Char
import Numeric
import Text.Parse.Units

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Data.UnitsOfMeasure.Internal
import Data.UnitsOfMeasure.Convert

-- | The 'u' quasiquoter may be used to create units or quantities;
-- its meaning depends on the context:
--
-- * in a declaration context, it creates new base and derived units
--   from a comma-separated list of names with optional definitions,
--   for example @['u'|kg, m, s, N = kg * m/s^2|]@;
--
-- * in a type context, it parses a single unit and converts it into
--   the corresponding type, so @['u'|m/s|]@ becomes the type
--   @'Base' "m" /: 'Base' "s"@ of kind 'Unit';
--
-- * in an expression context, it can be used to create a 'Quantity'
--   corresponding to a numeric literal, for example @['u'|42 m|]@ is
--   an expression of type @'Quantity' 'Integer' ('Base' "m")@,
--   @['u'|-2.2 m|]@ is an expression of type @'Quantity' 'Double' ('Base' "m")@,
--   and @['u'|m|]@ alone is a function of type @a -> 'Quantity' a ('Base' "m")@;
--
-- * in a pattern context, it can be used to match on a particular
--   value of a quantity with an 'Integer' or 'Rational'
--   representation type, for example @f ['u'| 42 m |] = 'True'@ is a
--   (partial) function of type @'Quantity' 'Integer' [u|m|] -> Bool@.
--
u :: QuasiQuoter
u = QuasiQuoter
      { quoteExp  = uExp
      , quotePat  = uPat
      , quoteType = uType
      , quoteDec  = uDec
      }

-- | Parse a unit expression optionally preceded by a literal, and
-- create a constructor for 'Quantity' with the given units (applied
-- to the literal if one is present).
uExp :: String -> Q Exp
uExp s
  | Just (ei, s') <- readNumber s = mkLiteral ei =<< parseUnitQ s'
  | otherwise                     = mkConversion =<< parseUnitQ s
  where
    mkLiteral (Left  0) Unity = [| zero |]
    mkLiteral (Right 0) Unity = [| MkQuantity 0.0 |]
    mkLiteral ei        expr  = [| (MkQuantity :: a -> Quantity a $(reifyUnit expr))
                                                                  $(litE (either integerL rationalL ei)) |]
    mkConversion expr = [|  MkQuantity :: a -> Quantity a $(reifyUnit expr) |]

-- | Parse an integer or rational literal followed by a unit
-- expression, and create a pattern match on @'Quantity' 'Integer' u@
-- or @'Quantity' 'Rational' u@.  Unfortunately we cannot easily
-- support arbitrary representation types.
uPat :: String -> Q Pat
uPat s
  | Just (Left  i, s') <- readNumber s  = mkPat (integerL  i) [t|Integer |] s'
  | Just (Right r, s') <- readNumber s  = mkPat (rationalL r) [t|Rational|] s'
  | otherwise                           = error "unable to parse literal"
  where
    mkPat l t s' = [p| MkQuantity $(litP l) |] `sigP` [t| Quantity $t $(uType s') |]

-- | Parse a unit expression and convert it into the corresponding type.
uType :: String -> Q Type
uType s = reifyUnit =<< parseUnitQ s

parseUnitQ :: String -> Q (UnitExp () String)
parseUnitQ s = case parseUnit universalSymbolTable s of
                 Right expr -> return expr
                 Left  err  -> fail ("unable to parse unit expression \"" ++ s ++ "\": " ++ err)

-- | Convert a unit expression into the corresponding type.
reifyUnit :: UnitExp () String -> Q Type
reifyUnit Unity        = [t| One |]
reifyUnit (Unit _ s)   = [t| MkUnit $(litT (strTyLit s))            |]
reifyUnit (u `Mult` v) = [t| $(reifyUnit u) *: $(reifyUnit v)       |]
reifyUnit (u `Div`  v) = [t| $(reifyUnit u) /: $(reifyUnit v)       |]
reifyUnit (u `Pow`  n) | n >= 0    = [t| $(reifyUnit u) ^: $(litT (numTyLit n)) |]
                       | otherwise = [t| One /: $(reifyUnit u) ^: $(litT (numTyLit (- n))) |]


-- | Parse the string as a mixture of base units and derived units,
-- and create corresponding 'MkUnit' type instance declarations.
uDec :: String -> Q [Dec]
uDec s = case parseUnitDecs s of
           Just xs -> concat <$> mapM (uncurry declareUnit) xs
           Nothing -> reportError ("unable to parse unit declarations: " ++ s) >> return []

data UnitDecl = BaseUnit
              | DefinedUnit    (UnitExp () String)
              | ConversionUnit Rational String

-- | Parse a comma-separated list of unit declarations, for example:
--
-- > kg, m, s, N = kg * m/s^2
parseUnitDecs :: String -> Maybe [(String, UnitDecl)]
parseUnitDecs = go
  where
    go [] = Just []
    go (c:xs) | isSpace c || c == ',' = go xs
    go xs = case span isAlpha xs of
              ([], _) -> Nothing
              (u, ys) -> go' u ys

    go' u [] = Just [(u, BaseUnit)]
    go' u (c:xs) | isSpace c = go' u xs
    go' u (',':xs) = ((u, BaseUnit) :) <$> go xs
    go' u ('=':xs) = let (d, ys) = break (== ',') xs
                     in case readNumber d of
                          Just (ei, s) -> case parseUnit universalSymbolTable s of
                                        Right (Unit _ e :: UnitExp () String) -> ((u, ConversionUnit (either fromInteger id ei) e) :) <$> go ys
                                        _                -> Nothing
                          _        -> case parseUnit universalSymbolTable d of
                                        Right e -> ((u, DefinedUnit e) :) <$> go ys
                                        Left  _ -> Nothing
    go' _ _        = Nothing

-- | Given a unit name and an optional definition, create an
-- appropriate instance of the 'MkUnit' type family.
declareUnit :: String -> UnitDecl -> Q [Dec]
declareUnit s ud = case ud of
  BaseUnit           -> [d| type instance MkUnit $(litT (strTyLit s)) = Base $(litT (strTyLit s))
                            instance HasCanonicalBaseUnit $(litT (strTyLit s))
                          |]
  DefinedUnit u      -> [d| type instance MkUnit $(litT (strTyLit s)) = $(reifyUnit u) |]
  ConversionUnit r t -> [d| type instance MkUnit $(litT (strTyLit s)) = Base $(litT (strTyLit s))
                            instance HasCanonicalBaseUnit $(litT (strTyLit s)) where
                              type CanonicalBaseUnit $(litT (strTyLit s)) = Base $(litT (strTyLit t))
                              conversionBase _ = MkQuantity $(litE (rationalL (recip r)))
                          |]

-- | Declare a canonical base unit of the given name, which must not
-- contain any spaces, e.g.
--
-- > declareBaseUnit "m"
--
-- produces
--
-- > type instance MkUnit "m" = Base "m"
-- > instance HasCanonicalBaseUnit "m"
--
-- This can also be written @['u'| m |]@.
declareBaseUnit :: String -> Q [Dec]
declareBaseUnit s = declareUnit s BaseUnit


-- | Declare a derived unit with the given name and definition, e.g.
--
-- > declareDerivedUnit "N" "kg m / s^2"
--
-- produces
--
-- > type instance MkUnit "N" = Base "kg" *: Base "m" /: Base "s" ^: 2
--
-- This can also be written @['u'| N = kg m / s^2 |]@.
declareDerivedUnit :: String -> String -> Q [Dec]
declareDerivedUnit s d = case parseUnit universalSymbolTable d of
                           Right e -> declareUnit s (DefinedUnit e)
                           Left _  -> reportError ("unable to parse derived unit: " ++ d) >> return []

-- | Declare a base unit of the given name, which is convertible to
-- the canonical base unit, e.g.
--
-- > declareConvertibleUnit "kilobyte" 1024 "byte"
--
-- produces
--
-- > type instance MkUnit "kilobyte" = Base "kilobyte"
-- > instance HasCanonicalBaseUnit "kilobyte" where
-- >   type CanonicalBaseUnit "kilobyte" = "byte"
-- >   conversionBase _ = [u| 1 % 1024 kilobyte/byte |]
--
-- This can also be written @['u'| kilobyte = 1024 byte |]@.
-- See "Data.UnitsOfMeasure.Convert" for more information about conversions.
declareConvertibleUnit :: String -> Rational -> String -> Q [Dec]
declareConvertibleUnit derived r base = declareUnit derived (ConversionUnit r base)


-- | Read either an integer or a rational from a string, if possible,
-- and return the remainder of the string.
readNumber :: String -> Maybe (Either Integer Rational, String)
readNumber s
  | [(r, s')] <- reads s                = Just (Right r, s')
  | [(i, s')] <- reads s                = Just (Left i , s')
  | [(r, s')] <- readSigned readFloat s = Just (Right r, s')
  | otherwise                           = Nothing
