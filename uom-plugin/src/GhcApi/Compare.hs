{-# LANGUAGE CPP #-}

module GhcApi.Compare
  ( cmpType
  , cmpTypes
  , cmpTyCon
  , thenCmp
  ) where

import GhcApi

import GHC.Utils.Fingerprint(Fingerprint(..), fingerprintString, fingerprintFingerprints)
import GHC.Utils.Misc (thenCmp)

import GHC.Types.Name
import GHC.Core.TyCon
import GHC.Unit.Module


-- TODO: all this is deeply dodgy!  These comparison functions are
-- non-deterministic, so we may end up getting different results on different
-- runs.  Really we should replace them with deterministic versions.

cmpTyCon :: TyCon -> TyCon -> Ordering
cmpTyCon x y = compare (fingerprintTyCon x) (fingerprintTyCon y)

cmpType :: Type -> Type -> Ordering
cmpType (TyConApp tc1 []) (TyConApp tc2 []) = cmpTyCon tc1 tc2
cmpType (LitTy x) (LitTy y) = cmpTyLit x y
cmpType t1 t2 = nonDetCmpType t1 t2

cmpTypes :: [Type] -> [Type] -> Ordering
cmpTypes [] [] = EQ
cmpTypes (t1:ts1) (t2:ts2) = cmpType t1 t2 `thenCmp` cmpTypes ts1 ts2
cmpTypes [] _ = LT
cmpTypes _ [] = GT


-- | Based on mkTyConRepTyConRHS from GHC.Tc.Instance.Typeable.  It is crucial
-- that this computes the same fingerprints (and hence the same ordering).
fingerprintTyCon :: TyCon -> Fingerprint
fingerprintTyCon tycon =
    fingerprintFingerprints [ pkg_fpr
                            , mod_fpr
                            , fingerprintString tycon_str
                            ]
  where
    tycon_str = add_tick (occNameString (getOccName tycon))
    add_tick s | isPromotedDataCon tycon = '\'' : s
               | otherwise               = s
    mod_fpr = fingerprintString $ moduleNameString $ moduleName md
    pkg_fpr = fingerprintString $ unitString $ moduleUnit md
    md = nameModule (getName tycon)
