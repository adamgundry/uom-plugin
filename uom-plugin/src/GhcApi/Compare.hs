{-# LANGUAGE CPP #-}

module GhcApi.Compare
  ( -- * GHC API changes
    cmpType
  , cmpTypes
  , cmpTyCon
  ) where

import GhcApi

#if __GLASGOW_HASKELL__ < 802
cmpTyCon :: TyCon -> TyCon -> Ordering
cmpTyCon = compare
#else
cmpType :: Type -> Type -> Ordering
cmpType = nonDetCmpType

cmpTypes :: [Type] -> [Type] -> Ordering
cmpTypes = nonDetCmpTypes

cmpTyCon :: TyCon -> TyCon -> Ordering
cmpTyCon a b = getUnique a `nonDetCmpUnique` getUnique b
#endif
