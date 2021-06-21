{-# LANGUAGE CPP #-}

module GhcApi
    ( module X
    ) where

import GHC.Core.Coercion as X (mkUnivCo, Coercion)

import GHC.Core.DataCon as X (dataConName, promoteDataCon, dataConWrapId)
import GHC.Data.FastString as X (FastString(..), fsLit)
import GHC as X (mkModuleName)
import GHC.Types.Name as X (mkSysTvName)
import GHC.Types.Name.Occurrence as X (occName, occNameFS, mkTcOcc)
import GHC.Utils.Outputable as X (Outputable(..), (<>), (<+>), ($$), text)
import GHC.Driver.Plugins as X (Plugin(..), defaultPlugin)

import GHC.Tc.Types.Evidence as X
    ( EvTerm(..)
    , TcCoercion
    , TcCoercionR
    , EvExpr
    , evCast
    , evDFunApp
    )

import GHC.Tc.Plugin as X
    ( TcPluginM
    , tcPluginTrace, matchFam, newFlexiTyVar, isTouchableTcPluginM
    , tcLookupTyCon, zonkCt
    , newUnique
    )
import GHC.Tc.Types as X (TcPlugin(..), TcPluginResult(..))
import GHC.Tc.Types.Constraint as X
    ( Ct(..), CtLoc
    , ctLoc, ctEvidence, ctEvPred, ctPred, ctEvExpr
    , isGiven, isWanted, isGivenCt
    , mkNonCanonical
    )
import GHC.Tc.Utils.TcType as X (tcSplitTyConApp_maybe, vanillaSkolemTv)
import GHC.Core.TyCon as X (TyCon(..), Role(..), isFamilyTyCon, tyConDataCons)

import GHC.Core.TyCo.Rep as X
    ( UnivCoProvenance(PluginProv)
    , Kind
    , Type(TyConApp, TyVarTy, AppTy, ForAllTy, FunTy, LitTy)
    , mkTyVarTy
    , mkFunTy
#if __GLASGOW_HASKELL__ > 900
    , cmpTyLit
#endif
    )

import GHC.Core.Coercion as X (mkPrimEqPred)
import GHC.Core.Predicate as X
    ( EqRel(..)
    , Pred(..)
    , classifyPredType
    )
import GHC as X (PredType)
import GHC.Core.Type as X
    ( splitTyConApp_maybe, typeKind
    , tyCoVarsOfType, tyCoVarsOfTypes
    , mkNumLitTy, mkTyConApp
    , isNumLitTy, isStrLitTy
    , coreView
    , mkStrLitTy
    , nonDetCmpType, nonDetCmpTypes, nonDetCmpTc
    , mkAppTy
    )

import GHC.Builtin.Types as X (typeSymbolKind, nilDataCon, consDataCon, heqTyCon, heqDataCon)

import GHC.Types.Unique as X
    ( getUnique
    , nonDetCmpUnique
    )

import GHC.Utils.Misc as X (thenCmp)

import GHC.Types.Var as X
    ( TyVar
    , DFunId
    , Id
    , mkTcTyVar
    )

import GHC.Types.Var.Set as X (TyCoVarSet, elemVarSet)
import GHC.TcPluginM.Extra as X (evByFiat, tracePlugin, lookupModule, lookupName )

import GHC.Driver.Plugins as X (PluginRecompile(..))
