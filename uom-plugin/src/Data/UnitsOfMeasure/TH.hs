{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Data.UnitsOfMeasure.TH
    ( u
    ) where

import GHC.Prim (Proxy#, proxy#)

import Control.Applicative
import Data.Char
import Text.Parse.Units

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Data.UnitsOfMeasure

-- | The 'u' quasiquoter may be used in a declaration, type or
-- expression context, and its meaning depends on the context:
--
-- * in a declaration context, it creates new base and derived units
--   from a comma-separated list of names with optional definitions,
--   for example @[u|kg, m, s, N = kg * m/s^2|]@;
--
-- * in a type context, it parses a single unit and converts it into
--   the corresponding type, so @[u|m/s|]@ becomes the type
--   @Base "m" /: Base "s"@ of kind 'Unit';
--
-- * in an expression context, it can be used to assign units to a
--   value, in conjunction with the '(%)' operator, e.g.
--   @42 % [u|m|]@ is an expression of type @Quantity Integer [u|m|]@
--
u :: QuasiQuoter
u = QuasiQuoter
      { quoteExp  = uExp
      , quotePat  = error "u quasiquoter cannot be used in a pattern context"
      , quoteType = uType
      , quoteDec  = uDec
      }

-- | Parse a unit expression and create a proxy that can be used with
-- the '(%)' operator to create a quantity with those units.
uExp :: String -> Q Exp
uExp s = [| proxy# :: Proxy# $(uType s) |]

-- | Parse a unit expression and convert it into the corresponding type.
uType :: String -> Q Type
uType s = case parseUnit universalSymbolTable s of
            Right expr -> reifyUnit expr
            Left  err  -> reportError ("unable to parse unit expression: " ++ err)  >> [t|()|]

-- | Convert a unit expression into the corresponding type.
reifyUnit :: UnitExp () String -> Q Type
reifyUnit Unity        = [t| One |]
reifyUnit (Unit _ s)   = [t| MkUnit $(litT (strTyLit s))            |]
reifyUnit (u `Mult` v) = [t| $(reifyUnit u) *: $(reifyUnit v)       |]
reifyUnit (u `Div`  v) = [t| $(reifyUnit u) /: $(reifyUnit v)       |]
reifyUnit (u `Pow`  n) = [t| $(reifyUnit u) ^: $(litT (numTyLit n)) |]


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
