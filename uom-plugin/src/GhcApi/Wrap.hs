{-# LANGUAGE CPP #-}

module GhcApi.Wrap
  ( -- * Wrappers
    newUnique
  , newWantedCt
  , newGivenCt
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
