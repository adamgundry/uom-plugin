{-# LANGUAGE CPP #-}

module TcPluginExtras
  ( -- * Wrappers
    newUnique
  , newWantedCt
  , newGivenCt

    -- * GHC API changes
  , cmpType
  , cmpTypes
  , cmpTyCon
  ) where

import GhcApi
import GHC.TcPluginM.Extra

#if __GLASGOW_HASKELL__ < 711
newUnique :: TcPluginM Unique
newUnique = unsafeTcPluginTcM TcRnMonad.newUnique
#endif

newWantedCt :: CtLoc -> PredType -> TcPluginM Ct
newWantedCt loc = fmap mkNonCanonical . newWanted loc

newGivenCt :: CtLoc -> PredType -> EvTerm -> TcPluginM Ct
newGivenCt loc prd ev = mkNonCanonical <$> newGiven loc prd ev

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
