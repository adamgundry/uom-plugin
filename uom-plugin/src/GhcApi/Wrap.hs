module GhcApi.Wrap
  ( -- * Wrappers
    newUnique
  , newWantedCt
  , newGivenCt
  ) where

import GhcApi
import GHC.TcPluginM.Extra

newWantedCt :: CtLoc -> PredType -> TcPluginM Ct
newWantedCt loc = fmap mkNonCanonical . newWanted loc

newGivenCt :: CtLoc -> PredType -> EvTerm -> TcPluginM Ct
newGivenCt loc prd ev = mkNonCanonical <$> newGiven loc prd ev
