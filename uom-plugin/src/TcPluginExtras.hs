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

import TcEvidence ( EvTerm )
import TcRnTypes  ( mkNonCanonical )
import TcRnMonad  ( Ct, CtLoc )
import Type       ( PredType, Type )
import TyCon      ( TyCon )
import Unique     ( getUnique, nonDetCmpUnique )

import GHC.TcPluginM.Extra

#if __GLASGOW_HASKELL__ < 711
import Unique     ( Unique )
import qualified TcRnMonad
import TcPluginM ( TcPluginM, unsafeTcPluginTcM )
#else
import TcPluginM ( TcPluginM, newUnique )
#endif

#if __GLASGOW_HASKELL__ < 802
import Type (cmpType, cmpTypes)
#else
import Type (nonDetCmpType, nonDetCmpTypes)
#endif


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
