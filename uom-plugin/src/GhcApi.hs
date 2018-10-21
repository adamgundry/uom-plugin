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
    , UnivCoProvenance(PluginProv), Type(..), Kind
    , mkTyVarTy

    -- * From Type
    , EqRel(..), PredTree(..), PredType
    , splitTyConApp_maybe, typeKind, classifyPredType
    , tyCoVarsOfType, tyCoVarsOfTypes
    , mkNumLitTy, mkTyConApp
    , isNumLitTy, isStrLitTy
    , coreView
    , mkPrimEqPred, mkStrLitTy

    -- * From TysWiredIn
    , typeSymbolKind, nilDataCon, consDataCon, heqTyCon, heqDataCon

    -- * From Var
    , TyVar
    , mkTcTyVar

    -- * From VarSet
    , TyCoVarSet
    , elemVarSet
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
    )
import TcRnTypes
    ( Ct(..), TcPlugin(..), TcPluginResult(..), CtLoc
    , ctLoc, ctEvidence, ctEvPred, ctPred
    , isGiven, isWanted, isGivenCt
    , mkNonCanonical
    )
import TcType (tcSplitTyConApp_maybe, vanillaSkolemTv)
import TyCon (TyCon(..), Role(..), isFamilyTyCon, tyConDataCons)
import TyCoRep (UnivCoProvenance(PluginProv), Type(..), Kind, mkTyVarTy)
import Type
    ( EqRel(..), PredTree(..), PredType
    , splitTyConApp_maybe, typeKind, classifyPredType
    , tyCoVarsOfType, tyCoVarsOfTypes
    , mkNumLitTy, mkTyConApp
    , isNumLitTy, isStrLitTy
    , coreView
    , mkPrimEqPred, mkStrLitTy
    )
import TysWiredIn (typeSymbolKind, nilDataCon, consDataCon, heqTyCon, heqDataCon)
import Var (TyVar, mkTcTyVar)
import VarSet (TyCoVarSet, elemVarSet)
