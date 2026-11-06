{-# LANGUAGE CPP #-}

-- | Re-exports of GHC functionality that is not provided by @ghc-tcplugin-api@.
module GhcApi
    ( module X
    ) where

import GHC.Types.Name as X (mkSysTvName)
import GHC.Utils.Outputable as X ((<>), (<+>), ($$), text)

import GHC.Tc.Utils.TcType as X (tcSplitTyConApp_maybe)
import GHC.Core.TyCon as X (isFamilyTyCon)

import GHC.Core.TyCo.Rep as X
    ( Type(TyConApp, TyVarTy, AppTy, ForAllTy, FunTy, LitTy, CastTy)
#if __GLASGOW_HASKELL__ > 900
    , cmpTyLit
#endif
    )

import GHC.Core.Type as X
    ( tyCoVarsOfType, tyCoVarsOfTypes
    , coreView
    )

import GHC.Core.Coercion as X
    ( MCoercion(MRefl, MCo), MCoercionN
    , coToMCo, mkTransMCo, mkSymMCo
    , mkCastTyMCo
    )

import GHC.Core.TyCo.FVs as X
    ( tyCoVarsOfMCo
    )

import GHC.Types.Unique as X
    ( getUnique
    , nonDetCmpUnique
    )

import GHC.Types.Var.Set as X (elemVarSet, unionVarSet)
