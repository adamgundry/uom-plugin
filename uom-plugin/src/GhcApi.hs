{-# LANGUAGE CPP #-}

module GhcApi
    (
    -- * From Coercion
      mkUnivCo

    -- * From DataCon
    , dataConName, promoteDataCon, dataConWrapId

    -- * From FastString
    , FastString(..)
    , fsLit

    -- * From Module
    , mkModuleName

    -- * From Name
    , mkSysTvName

    -- * From OccName
    , occName, occNameFS, mkTcOcc

    -- * From Outputable
    , Outputable(..)
    , (<>), (<+>), ($$)
    , text

    -- * From Plugins
    , Plugin(..)
    , defaultPlugin

    -- * From TcEvidence
    , EvTerm(..)

    -- * From TcPluginM
    , TcPluginM
    , tcPluginTrace, matchFam, newFlexiTyVar, isTouchableTcPluginM
    , tcLookupTyCon, zonkCt
    , newUnique

    -- * From TcRnTypes
    , Ct(..), TcPlugin(..), TcPluginResult(..), CtLoc
    , ctLoc, ctEvidence, ctEvPred, ctPred
    , isGiven, isWanted, isGivenCt
    , mkNonCanonical

    -- * From TcType
    , tcSplitTyConApp_maybe, vanillaSkolemTv

    -- * From TyCon
    , TyCon(..), Role(..)
    , isFamilyTyCon, tyConDataCons

    -- * From TyCoRep
    , UnivCoProvenance(PluginProv)
    , Kind
#if __GLASGOW_HASKELL__ >= 802
    , Type(TyConApp, TyVarTy, AppTy, ForAllTy, FunTy)
#elif __GLASGOW_HASKELL__ >= 800
    , Type(TyConApp, TyVarTy, AppTy, ForAllTy)
#endif
#if __GLASGOW_HASKELL__ <= 800
    , TyBinder(Anon)
#endif
    , mkTyVarTy

    -- * From Type
    , EqRel(..), PredTree(..), PredType
    , splitTyConApp_maybe, typeKind, classifyPredType
    , tyCoVarsOfType, tyCoVarsOfTypes
    , mkNumLitTy, mkTyConApp
    , isNumLitTy, isStrLitTy
    , coreView
    , mkPrimEqPred, mkStrLitTy
#if __GLASGOW_HASKELL__ >= 802
    , nonDetCmpType, nonDetCmpTypes
#endif

    -- * From TysWiredIn
    , typeSymbolKind, nilDataCon, consDataCon, heqTyCon, heqDataCon

    -- * From Unique
    , getUnique
#if __GLASGOW_HASKELL__ >= 802
    , nonDetCmpUnique
#endif

    -- * From Util
    , thenCmp

    -- * From Var
    , TyVar
    , mkTcTyVar

    -- * From VarSet
    , TyCoVarSet
    , elemVarSet

    -- * From GHC.TcPluginM.Extra
    , evByFiat, tracePlugin, lookupModule, lookupName
    ) where

import Coercion (mkUnivCo)
import DataCon (dataConName, promoteDataCon, dataConWrapId)
import FastString (FastString(..), fsLit)
import Module (mkModuleName)
import Name (mkSysTvName)
import OccName (occName, occNameFS, mkTcOcc)
import Outputable (Outputable(..), (<>), (<+>), ($$), text)
import Plugins (Plugin(..), defaultPlugin)
import TcEvidence (EvTerm(..))
import TcPluginM
    ( TcPluginM
    , tcPluginTrace, matchFam, newFlexiTyVar, isTouchableTcPluginM
    , tcLookupTyCon, zonkCt
    , newUnique
    )
import TcRnTypes
    ( Ct(..), TcPlugin(..), TcPluginResult(..), CtLoc
    , ctLoc, ctEvidence, ctEvPred, ctPred
    , isGiven, isWanted, isGivenCt
    , mkNonCanonical
    )
import TcType (tcSplitTyConApp_maybe, vanillaSkolemTv)
import TyCon (TyCon(..), Role(..), isFamilyTyCon, tyConDataCons)

import TyCoRep
    ( UnivCoProvenance(PluginProv)
    , Kind
#if __GLASGOW_HASKELL__ >= 802
    , Type(TyConApp, TyVarTy, AppTy, ForAllTy, FunTy)
#elif __GLASGOW_HASKELL__ >= 800
    , Type(TyConApp, TyVarTy, AppTy, ForAllTy)
#endif
#if __GLASGOW_HASKELL__ <= 800
    , TyBinder(Anon)
#endif
    , mkTyVarTy
    )

import Type
    ( EqRel(..), PredTree(..), PredType
    , splitTyConApp_maybe, typeKind, classifyPredType
    , tyCoVarsOfType, tyCoVarsOfTypes
    , mkNumLitTy, mkTyConApp
    , isNumLitTy, isStrLitTy
    , coreView
    , mkPrimEqPred, mkStrLitTy
#if __GLASGOW_HASKELL__ >= 802
    , nonDetCmpType, nonDetCmpTypes
#endif
    )

import TysWiredIn (typeSymbolKind, nilDataCon, consDataCon, heqTyCon, heqDataCon)

import Unique
    ( getUnique
#if __GLASGOW_HASKELL__ >= 802
    , nonDetCmpUnique
#endif
    )

import Util (thenCmp)
import Var (TyVar, mkTcTyVar)
import VarSet (TyCoVarSet, elemVarSet)
import GHC.TcPluginM.Extra (evByFiat, tracePlugin, lookupModule, lookupName )
