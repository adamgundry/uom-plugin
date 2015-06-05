{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module TcPluginExtras
  ( -- * Wrappers
    newUnique
  , newWantedCt
  , newGivenCt
  ) where

import TcPluginM  ( TcPluginM )
import TcEvidence ( EvTerm )
import TcRnTypes  ( mkNonCanonical )
import TcRnMonad  ( Ct, CtLoc )
import Type       ( PredType )

import GHC.TcPluginM.Extra

#if __GLASGOW_HASKELL__ < 711
import Unique     ( Unique )
import qualified TcRnMonad
import TcPluginM ( unsafeTcPluginTcM )
#else
import TcPluginM ( newUnique )
#endif


#if __GLASGOW_HASKELL__ < 711
newUnique :: TcPluginM Unique
newUnique = unsafeTcPluginTcM TcRnMonad.newUnique
#endif

newWantedCt :: CtLoc -> PredType -> TcPluginM Ct
newWantedCt loc = fmap mkNonCanonical . newWanted loc

newGivenCt :: CtLoc -> PredType -> EvTerm -> TcPluginM Ct
newGivenCt loc prd ev = fmap mkNonCanonical $ newGiven loc prd ev
