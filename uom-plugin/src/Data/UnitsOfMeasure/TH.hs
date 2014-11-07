{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Data.UnitsOfMeasure.TH
    ( u
    ) where

import GHC.Prim (Proxy#, proxy#)

import Text.Parse.Units

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
uType s = case parseUnit universalSymbolTable s of
            Right expr -> reifyUnit expr
            Left  err  -> reportError ("unable to parse unit expression: " ++ err)  >> [t|()|]

reifyUnit :: UnitExp () String -> Q Type
reifyUnit Unity        = [t| One |]
reifyUnit (Unit _ s)   = [t| MkUnit $(litT (strTyLit s))            |]
reifyUnit (u `Mult` v) = [t| $(reifyUnit u) *: $(reifyUnit v)       |]
reifyUnit (u `Div`  v) = [t| $(reifyUnit u) /: $(reifyUnit v)       |]
reifyUnit (u `Pow`  n) = [t| $(reifyUnit u) ^: $(litT (numTyLit n)) |]
