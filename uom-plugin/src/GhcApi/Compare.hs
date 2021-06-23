{-# LANGUAGE CPP #-}

module GhcApi.Compare
  ( -- * GHC API changes
    cmpType
  , cmpTypes
  , cmpTyCon
  ) where

import GhcApi

-- TODO: all this is deeply dodgy!  These comparison functions are
-- non-deterministic, so we may end up getting different results on different
-- runs.  Really we should replace them with deterministic versions.

cmpTyCon :: TyCon -> TyCon -> Ordering
cmpTyCon = nonDetCmpTc

cmpType :: Type -> Type -> Ordering
#if __GLASGOW_HASKELL__ > 900
cmpType (LitTy x) (LitTy y) = cmpTyLit x y
cmpType t1 t2 = nonDetCmpType t1 t2
#else
cmpType = nonDetCmpType
#endif

cmpTypes :: [Type] -> [Type] -> Ordering
cmpTypes [] [] = EQ
cmpTypes (t1:ts1) (t2:ts2) = cmpType t1 t2 `thenCmp` cmpTypes ts1 ts2
cmpTypes [] _ = LT
cmpTypes _ [] = GT
