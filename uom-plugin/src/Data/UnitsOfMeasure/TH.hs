{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Data.UnitsOfMeasure.TH
    ( u
    ) where

import GHC.Prim (Proxy#, proxy#)

import Control.Applicative

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Data.UnitsOfMeasure

u :: QuasiQuoter
u = QuasiQuoter
      { quoteExp  = uExp
      , quotePat  = error "u quasiquoter cannot be used in a pattern context"
      , quoteType = uType
      , quoteDec  = error "u quasiquoter cannot be used in a declaration context"
      }

uExp :: String -> Q Exp
uExp s = [| proxy# :: Proxy# $(uType s) |]

uType :: String -> Q Type
uType s = case parseUnit s of
            Just expr -> reifyUnit expr
            Nothing   -> reportError ("unable to parse unit expression: " ++ s)
                         >> [t|()|]

data UnitExpr = OneExpr
              | BaseExpr String
              | UnitExpr :*: UnitExpr
              | UnitExpr :/: UnitExpr
              | UnitExpr :^: Integer

-- | This is a quick hack, not a proper parser for unit expressions!
parseUnit :: String -> Maybe UnitExpr
parseUnit = go ""
  where
    go :: String -> String -> Maybe UnitExpr
    go x  [] = mkExpr x
    go "" (' ':s) = go "" s
    go x  (' ':s) = (BaseExpr x :*:) <$> go "" s
    go x  ('*':s) = (:*:) <$> mkExpr x <*> go "" s
    go x  ('/':s) = (:/:) <$> mkExpr x <*> go "" s
    go x  ('^':s) = (:^:) <$> mkExpr x <*> readMay s
    go x  (c:s)   = go (c:x) s

    readMay s = case reads s of
                  [(x, "")] -> Just x
                  _         -> Nothing

    mkExpr :: String -> Maybe UnitExpr
    mkExpr "" = Nothing
    mkExpr x  = Just $ BaseExpr $ reverse x

reifyUnit :: UnitExpr -> Q Type
reifyUnit OneExpr      = [t|One|]
reifyUnit (BaseExpr s) = [t| MkUnit $(litT (strTyLit s)) |]
reifyUnit (u :*: v)    = [t| $(reifyUnit u) *: $(reifyUnit v) |]
reifyUnit (u :/: v)    = [t| $(reifyUnit u) /: $(reifyUnit v) |]
reifyUnit (u :^: n)    = [t| $(reifyUnit u) ^: $(litT (numTyLit n)) |]
