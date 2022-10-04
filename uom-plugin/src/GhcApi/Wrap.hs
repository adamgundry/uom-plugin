module GhcApi.Wrap
  ( -- * Wrappers
    newUnique
  , newWantedCt
  , newGivenCt
  ) where

import GhcApi
import GHC.TcPluginM.Extra

newWantedCt :: CtLoc -> PredType -> TcPluginM Ct
newWantedCt loc = undefined -- fmap mkNonCanonical . newWanted loc

newGivenCt :: CtLoc -> PredType -> EvTerm -> TcPluginM Ct
newGivenCt loc prd ev = undefined -- mkNonCanonical <$> newGiven loc prd ev
