{-# LANGUAGE CPP #-}

module GhcApi.Compare
  ( -- * GHC API changes
    cmpType
  , cmpTypes
  , cmpTyCon
  ) where

import GhcApi
#if __GLASGOW_HASKELL__ < 802
import qualified Type (cmpType, cmpTypes)
#endif

cmpTyCon :: TyCon -> TyCon -> Ordering
#if __GLASGOW_HASKELL__ >= 802
cmpTyCon a b = getUnique a `nonDetCmpUnique` getUnique b
#else
cmpTyCon = compare
#endif

cmpType :: Type -> Type -> Ordering
cmpType =
#if __GLASGOW_HASKELL__ >= 802
    nonDetCmpType
#else
    Type.cmpType
#endif

cmpTypes :: [Type] -> [Type] -> Ordering
cmpTypes =
#if __GLASGOW_HASKELL__ >= 802
    nonDetCmpTypes
#else
    Type.cmpTypes
#endif
