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
import Data.Ratio
import Numeric
import Text.Parse.Units

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Data.UnitsOfMeasure.Internal
import Data.UnitsOfMeasure.Convert
import Data.UnitsOfMeasure.Singleton

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
reifyUnit (Unit _ s)   = conT (toUnitName s)
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
              | ConversionUnit Rational (UnitExp () String)

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
                          Just (ei, s)
                            | not (all isSpace s) -- parse "x = 1" as DefinedUnit, not ConversionUnit
                              -> case parseUnit universalSymbolTable s of
                                   Right e -> ((u, ConversionUnit (either fromInteger id ei) e) :) <$> go ys
                                   _                -> Nothing
                          _   -> case parseUnit universalSymbolTable d of
                                   Right e -> ((u, DefinedUnit e) :) <$> go ys
                                   Left  _ -> Nothing
    go' _ _        = Nothing

-- | Given a unit name and an optional definition, create an
-- appropriate type and class instances.
declareUnit :: String -> UnitDecl -> Q [Dec]
declareUnit s ud = do
    unitType <- [t| Unit |]
    let con = conT (toUnitName s)
    let val = toUnitValueName s
    sequence $ case ud of
        BaseUnit ->
            [ dataD (pure []) (toUnitName s) [] (Just unitType) [] []
            , instanceD (pure []) [t| HasCanonicalBaseUnit $con |]
                [ do a <- varT <$> newName "a"
                     tySynInstD (tySynEqn Nothing [t| ConversionRatioConstraints $con $a |] [t| Num $a|]) ]
            , instanceD (pure []) [t| KnownBaseUnit $con |]
                [ valD (varP 'baseUnitName) (normalB (stringE s)) [] ]
            , sigD val [t| forall a . Num a => Quantity a $con |]
            , valD (varP val) (normalB [e| MkQuantity 1 |]) []
            ]
        DefinedUnit u ->
            [ tySynD (toUnitName s) [] (unitExpToType u)
            , sigD val [t| forall a . Num a => Quantity a $con |]
            , valD (varP val) (normalB [e| MkQuantity 1 |]) []
            ]
        ConversionUnit _ (Unit Nothing s')
            | s == s' -> [ do reportError ("cannot define cyclic convertible unit: " ++ s)
                              dataD (pure []) (toUnitName s) [] (Just unitType) [] []  ]
        ConversionUnit r u ->
            [ dataD (pure []) (toUnitName s) [] (Just unitType) [] []
            , instanceD (pure []) [t| HasCanonicalBaseUnit $con |]
                  [ tySynInstD (tySynEqn Nothing [t| CanonicalBaseUnit $con |] (unitExpToType u)
                               )
                  , valD (varP 'conversionBase) (normalB
                            (if denominator r == 1 then [e| MkQuantity $(lift (numerator r)) |] else [e| MkQuantity r |])) []
                  , do a <- varT <$> newName "a"
                       tySynInstD (tySynEqn Nothing [t| ConversionRatioConstraints $con $a |]
                                    (if denominator r == 1 then [t| Num $a |] else [t| Fractional $a|]))
                  ]
            , instanceD (pure []) [t| KnownBaseUnit $con |]
                  [ valD (varP 'baseUnitName) (normalB (stringE s)) [] ]
            , sigD val [t| forall a . Num a => Quantity a $con |]
            , valD (varP val) (normalB [e| MkQuantity 1 |]) []
            ]

-- | Convert a unit name into a type constructor name.
toUnitName :: String -> Name
toUnitName s = mkName $ "U_" ++ s

-- | Convert a unit name into a value-level name.
toUnitValueName :: String -> Name
toUnitValueName = mkName . mangleUnitValueName

-- | Transform the name of a unit so it is suitable for use as a value-level
-- binding. In particular we need to prefix unit names with an underscore if
-- they start with a capital letter or form a keyword.
mangleUnitValueName :: String -> String
mangleUnitValueName ss@(s:_)
  | isUpper s || ss `elem` keywords  = '_':ss
mangleUnitValueName s = s

keywords :: [String]
keywords = [ "case", "class", "data", "default", "deriving", "do", "else", "foreign", "if", "import", "in", "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where" ]

unitExpToType :: UnitExp () String -> Q Type
unitExpToType Unity = [t| One |]
unitExpToType (Unit _ u) = conT (toUnitName u)
unitExpToType (Mult u v) = [t| $(unitExpToType u) *: $(unitExpToType v) |]
unitExpToType (Div u v) = [t| $(unitExpToType u) /: $(unitExpToType v) |]
unitExpToType (Pow u n)
  | n >= 0    = [t| $(unitExpToType u) ^: $(natToType n) |]
  | otherwise = [t| One /: ($(unitExpToType u) ^: $(natToType (abs n))) |]

natToType :: Integer -> Q Type -- TODO Natural
natToType = litT . numTyLit


-- | Declare a canonical base unit of the given name, which must not
-- contain any spaces, e.g.
--
-- > declareBaseUnit "m"
--
-- produces
--
-- > data U_m :: Unit
-- > instance HasCanonicalBaseUnit U_m
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
-- > type U_N = U_kg *: U_m /: U_s ^: 2
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
-- > data U_kilobyte :: Unit
-- > instance HasCanonicalBaseUnit U_kilobyte where
-- >   type CanonicalBaseUnit U_kilobyte = U_byte
-- >   conversionBase = [u| 1 % 1024 kilobyte/byte |]
--
-- This can also be written @['u'| kilobyte = 1024 byte |]@.
-- See "Data.UnitsOfMeasure.Convert" for more information about conversions.
declareConvertibleUnit :: String -> Rational -> String -> Q [Dec]
declareConvertibleUnit derived r base =  case parseUnit universalSymbolTable base of
    Right e -> declareUnit derived (ConversionUnit r e)
    Left _  -> reportError ("unable to parse convertible unit: " ++ base) >> return []


-- | Read either an integer or a rational from a string, if possible,
-- and return the remainder of the string.
readNumber :: String -> Maybe (Either Integer Rational, String)
readNumber s
  | [(r, s')] <- reads s                = Just (Right r, s')
  | [(i, s')] <- reads s                = Just (Left i , s')
  | [(r, s')] <- readSigned readFloat s = Just (Right r, s')
  | otherwise                           = Nothing
