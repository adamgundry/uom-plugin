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
    ) where

import Data.Char
import Numeric
import Text.Parse.Units

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Data.UnitsOfMeasure.Internal

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
  | [(r, s')] <- reads s                = mkLiteral (rationalL r) s'
  | [(i, s')] <- reads s                = mkLiteral (integerL  i) s'
  | [(r, s')] <- readSigned readFloat s = mkLiteral (rationalL r) s'
  | otherwise                           = mkConversion =<< parseUnitQ s
  where
    mkLiteral l s'    = [| (MkQuantity :: a -> Quantity a $(uType s'      )) $(litE l) |]
    mkConversion expr = [|  MkQuantity :: a -> Quantity a $(reifyUnit expr) |]

-- | Parse an integer or rational literal followed by a unit
-- expression, and create a pattern match on @'Quantity' 'Integer' u@
-- or @'Quantity' 'Rational' u@.  Unfortunately we cannot easily
-- support arbitrary representation types.
uPat :: String -> Q Pat
uPat s
  | [(r, s')] <- reads s                = mkPat (rationalL r) [t|Rational|] s'
  | [(i, s')] <- reads s                = mkPat (integerL  i) [t|Integer |] s'
  | [(r, s')] <- readSigned readFloat s = mkPat (rationalL r) [t|Rational|] s'
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

-- | Parse a comma-separated list of unit declarations, for example:
--
-- > kg, m, s, N = kg * m/s^2
parseUnitDecs :: String -> Maybe [(String, Maybe (UnitExp () String))]
parseUnitDecs = go
  where
    go [] = Just []
    go (c:xs) | isSpace c || c == ',' = go xs
    go xs = case span isAlpha xs of
              ([], _) -> Nothing
              (u, ys) -> go' u ys

    go' u [] = Just [(u, Nothing)]
    go' u (c:xs) | isSpace c = go' u xs
    go' u (',':xs) = ((u, Nothing) :) <$> go xs
    go' u ('=':xs) = let (d, ys) = break (== ',') xs
                     in case parseUnit universalSymbolTable d of
                          Right e -> ((u, Just e) :) <$> go ys
                          Left  _ -> Nothing
    go' _ _        = Nothing

-- | Given a unit name and an optional definition, create an
-- appropriate instance of the 'MkUnit' type family.
declareUnit :: String -> Maybe (UnitExp () String) -> Q [Dec]
declareUnit s Nothing  = [d| type instance MkUnit $(litT (strTyLit s)) = Base $(litT (strTyLit s)) |]
declareUnit s (Just u) = [d| type instance MkUnit $(litT (strTyLit s)) = $(reifyUnit u)            |]

-- | Declare a base unit of the given name, which must not contain any
-- spaces, e.g. @declareBaseUnit "m"@.
declareBaseUnit :: String -> Q [Dec]
declareBaseUnit s = declareUnit s Nothing

-- | Declare a derived unit with the given name and definition, e.g.
-- @declareDerivedUnit "N" "kg m / s^2"@.
declareDerivedUnit :: String -> String -> Q [Dec]
declareDerivedUnit s d = case parseUnit universalSymbolTable d of
                           Right e -> declareUnit s (Just e)
                           Left _  -> reportError ("unable to parse derived unit: " ++ d) >> return []
