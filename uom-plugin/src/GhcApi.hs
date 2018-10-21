module GhcApi
    ( -- * From TyCon
      TyCon(..)
    , isFamilyTyCon

    -- * From Type
    , EqRel(..), PredTree(..)
    , splitTyConApp_maybe, typeKind, classifyPredType
    , tyCoVarsOfType, tyCoVarsOfTypes
    , mkNumLitTy, mkTyConApp
    , isNumLitTy, isStrLitTy
    , coreView

    -- * From TcType
    , tcSplitTyConApp_maybe, vanillaSkolemTv

    -- * From TyCoRep
    , Type(..), Kind
    , mkTyVarTy

    -- * From Name
    , mkSysTvName

    -- * From Var
    , TyVar
    , mkTcTyVar

    -- * From VarSet
    , TyCoVarSet
    , elemVarSet

    -- * From FastString
    , FastString(..)
    , fsLit

    -- * From Outputable
    , Outputable(..)
    , (<>), (<+>), ($$)
    , text

    -- * From TcRnMonad
    , Ct
    , isGiven, ctEvidence, ctEvPred

    -- * From TcPluginM
    , TcPluginM
    , tcPluginTrace, matchFam, newFlexiTyVar, isTouchableTcPluginM
    ) where

import Type
    ( EqRel(..), PredTree(..)
    , splitTyConApp_maybe, typeKind, classifyPredType
    , tyCoVarsOfType, tyCoVarsOfTypes
    , mkNumLitTy, mkTyConApp
    , isNumLitTy, isStrLitTy
    , coreView
    )
import TyCon (TyCon(..), isFamilyTyCon)
import TcType (tcSplitTyConApp_maybe, vanillaSkolemTv)
import TyCoRep (Type(..), Kind, mkTyVarTy)
import Name (mkSysTvName)
import Var (TyVar, mkTcTyVar)
import VarSet (TyCoVarSet, elemVarSet)
import FastString (FastString(..), fsLit)
import Outputable (Outputable(..), (<>), (<+>), ($$), text)
import TcRnMonad (Ct, isGiven, ctEvidence, ctEvPred)
import TcPluginM (TcPluginM, tcPluginTrace, matchFam, newFlexiTyVar, isTouchableTcPluginM)


