{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module TcPluginExtras
  ( -- * Re-exports from GHC typechecker plugin API
    TcPlugin(..)
  , TcPluginSolver
  , TcPluginM
  , tcPluginTrace
  , unsafeTcPluginTcM
  , tcLookupClass
  , tcLookupTyCon
  , newFlexiTyVar
  , isTouchableTcPluginM
  , zonkCt
  , matchFam

    -- * Wrappers
  , newUnique
  , newWantedCt
  , newGivenCt

    -- * Extensions
  , tracePlugin
  , lookupModule
  , lookupName
  ) where

import TcPluginM

import TcEvidence ( EvTerm )
import TcRnTypes  ( TcPlugin(..), TcPluginSolver, mkNonCanonical )
import TcRnMonad  ( Ct, CtLoc )
import Type       ( PredType )
import Unique     ( Unique )

import qualified TcRnMonad

import GHC.TcPluginM.Extra

#if __GLASGOW_HASKELL__ >= 711

#else
import TcMType ( newSimpleWanted )
#endif


newUnique :: TcPluginM Unique
newUnique = unsafeTcPluginTcM TcRnMonad.newUnique

newWantedCt :: CtLoc -> PredType -> TcPluginM Ct
#if __GLASGOW_HASKELL__ >= 711
newWantedCt loc = fmap mkNonCanonical . TcPluginM.newWanted loc
#else
newWantedCt loc = unsafeTcPluginTcM . TcMType.newSimpleWanted (TcRnMonad.ctLocOrigin loc)
#endif

newGivenCt :: CtLoc -> PredType -> EvTerm -> TcPluginM Ct
#if __GLASGOW_HASKELL__ >= 711
newGivenCt loc prd ev = fmap mkNonCanonical $ TcPluginM.newGiven loc prd ev
#else
newGivenCt loc prd ev = return $ mkNonCanonical $ TcRnMonad.CtGiven prd ev loc
#endif
