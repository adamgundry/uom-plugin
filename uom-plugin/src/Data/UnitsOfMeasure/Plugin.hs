-- | This module defines a typechecker plugin that solves equations
-- involving units of measure.  To use it, add
--
-- > {-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
--
-- above the module header of your source files, or in the
-- @ghc-options@ field of your @.cabal@ file.  You do not need to
-- import this module.
module Data.UnitsOfMeasure.Plugin
  ( plugin
  ) where

import Plugins

import TcEvidence
import TcRnTypes
import TcType

import Coercion
import BasicTypes
import DataCon
import Type
import TyCon
import TypeRep
import TysWiredIn

import FastString
import Outputable

import OccName ( occName, occNameFS, mkTcOcc )
import Module

import Data.Either

import Data.UnitsOfMeasure.Plugin.Convert
import Data.UnitsOfMeasure.Plugin.NormalForm
import Data.UnitsOfMeasure.Plugin.Unify
import TcPluginExtras


-- | The plugin that GHC will load when this module is used with the
-- @-fplugin@ option.
plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const $ Just uomPlugin }

uomPlugin :: TcPlugin
uomPlugin = tracePlugin "uom-plugin" $ TcPlugin { tcPluginInit  = lookupUnitDefs
                                               , tcPluginSolve = unitsOfMeasureSolver
                                               , tcPluginStop  = const $ return ()
                                               }


unitsOfMeasureSolver :: UnitDefs -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
unitsOfMeasureSolver uds givens _deriveds []      = do
    zonked_cts <- mapM zonkCt givens
    let (unit_givens , _) = partitionEithers $ zipWith foo givens $ map (toUnitEquality uds) zonked_cts
    case unit_givens of
      []    -> return $ TcPluginOk [] []
      (_:_) -> do
        sr <- simplifyUnits uds $ map snd unit_givens
        tcPluginTrace "unitsOfMeasureSolver simplified givens only" $ ppr sr
        return $ case sr of
          -- Simplified tvs []    evs eqs -> TcPluginOk (map (solvedGiven . fst) unit_givens) []
          Simplified _    -> TcPluginOk [] []
          Impossible eq _ -> TcPluginContradiction [fromUnitEquality eq]
  where
    foo :: Ct -> Either UnitEquality Ct -> Either (Ct, UnitEquality) Ct
    foo ct (Left x)    = Left (ct, x)
    foo _  (Right ct') = Right ct'

    solvedGiven ct = (ctEvTerm (ctEvidence ct), ct)


unitsOfMeasureSolver uds givens _deriveds wanteds
  | xs@(_:_) <- lookForUnpacks uds givens wanteds = return $ TcPluginOk [] xs
  | otherwise = do
    let (unit_wanteds, _) = partitionEithers $ map (toUnitEquality uds) wanteds
    case unit_wanteds of
      []    -> return $ TcPluginOk [] []
      (_:_) -> do
        (unit_givens , _) <- partitionEithers . map (toUnitEquality uds) <$> mapM zonkCt givens
        sr <- simplifyUnits uds unit_givens
        tcPluginTrace "unitsOfMeasureSolver simplified givens" $ ppr sr
        case sr of
          Impossible eq _ -> return $ TcPluginContradiction [fromUnitEquality eq]
          Simplified ss   -> do sr' <- simplifyUnits uds $ map (substsUnitEquality (simplifySubst ss)) unit_wanteds
                                tcPluginTrace "unitsOfMeasureSolver simplified wanteds" $ ppr sr'
                                case sr' of
                                  Impossible eq _ -> return $ TcPluginContradiction [fromUnitEquality $ substsUnitEquality (simplifyUnsubst ss) eq]
                                  Simplified ss'  -> TcPluginOk [ (evMagic uds ct, ct) | eq <- simplifySolved ss', let ct = fromUnitEquality eq ]
                                                         <$> mapM (substItemToCt uds) (filter (isWanted . ctEvidence . siCt) (substsSubst (simplifyUnsubst ss) (simplifySubst ss')))


substItemToCt :: UnitDefs -> SubstItem -> TcPluginM Ct
substItemToCt uds si
      | isGiven (ctEvidence ct) = return $ mkNonCanonical $ CtGiven pred (evByFiat "units" (ty1, ty2)) loc
      | otherwise               = newSimpleWanted (ctLocOrigin loc) pred
      where
        pred = mkEqPred ty1 ty2
        ty1  = mkTyVarTy (siVar si)
        ty2  = reifyUnit uds (siUnit si)
        ct   = siCt si
        loc  = ctLoc ct


lookForUnpacks :: UnitDefs -> [Ct] -> [Ct] -> [Ct]
lookForUnpacks uds givens wanteds = map unpackCt unpacks
  where
    unpacks = concatMap collectCt $ givens ++ wanteds

    collectCt ct = collectType ct $ ctEvPred $ ctEvidence ct

    collectType _  (TyVarTy v)      = []
    collectType ct (AppTy f s)      = collectType ct f ++ collectType ct s
    collectType ct (TyConApp tc [a])
      | tc == unpackTyCon uds       = case maybeConstant =<< normaliseUnit uds a of
                                        Just xs -> [(ct,a,xs)]
                                        _       -> []
    collectType ct (TyConApp tc as) = concatMap (collectType ct) as
    collectType ct (FunTy t v)      = collectType ct t ++ collectType ct v
    collectType ct (ForAllTy _ t)   = collectType ct t
    collectType _  (LitTy _)        = []

    unpackCt (ct,a,xs) = mkNonCanonical $ CtGiven (mkEqPred ty1 ty2) (evByFiat "units" (ty1, ty2)) loc
      where
        ty1 = TyConApp (unpackTyCon uds) [a]
        ty2 = foldr promoter (mkTyConApp (promoteDataCon nilDataCon) [list_elem_ty]) xs
        loc = ctLoc ct

    promoter (b, i) t = mkTyConApp cons_tycon [ list_elem_ty, mkTyConApp pair_con [typeSymbolKind, typeIntKind, mkStrLitTy b, mkTypeInt i], t]
    list_elem_ty = mkTyConApp pair_tycon [typeSymbolKind, typeIntKind]
    cons_tycon = promoteDataCon consDataCon
    pair_tycon = promotedTupleTyCon   BoxedTuple 2
    pair_con   = promotedTupleDataCon BoxedTuple 2
    typeIntKind = mkTyConTy $ promoteTyCon $ typeIntTyCon uds

    mkTypeInt i | i >= 0    = mkTyConApp (typeIntPosTyCon uds) [mkNumLitTy i]
                | otherwise = mkTyConApp (typeIntNegTyCon uds) [mkNumLitTy (-i)]


-- Extract the unit equality constraints
toUnitEquality :: UnitDefs -> Ct -> Either UnitEquality Ct
toUnitEquality uds ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2
      | isUnitKind uds (typeKind t1) || isUnitKind uds (typeKind t1)
      , Just u1 <- normaliseUnit uds t1
      , Just u2 <- normaliseUnit uds t2 -> Left (ct, u1, u2)
    IrredPred t
      | Just (tc, [t1,t2]) <- splitTyConApp_maybe t
      , tc == equivTyCon uds
      , Just u1 <- normaliseUnit uds t1
      , Just u2 <- normaliseUnit uds t2 -> Left (ct, u1, u2)
    _                                   -> Right ct

fromUnitEquality :: UnitEquality -> Ct
fromUnitEquality (ct, _, _) = ct


lookupUnitDefs :: TcPluginM UnitDefs
lookupUnitDefs = do
    md <- lookupModule myModule myPackage
    u <- look md "Unit"
    b <- look md "Base"
    o <- look md "One"
    m <- look md "*:"
    d <- look md "/:"
    e <- look md "^:"
    x <- look md "Unpack"
    i <- look md "TypeInt"
    c <- look md "~~"
    return $ UnitDefs u b o m d e x i (getDataCon i "Pos") (getDataCon i "Neg") c
  where
    getDataCon u s = case [ dc | dc <- tyConDataCons u, occNameFS (occName (dataConName dc)) == fsLit s ] of
                       [d] -> promoteDataCon d
                       _   -> error $ "lookupUnitDefs/getDataCon: missing " ++ s

    look md s = tcLookupTyCon =<< lookupName md (mkTcOcc s)
    myModule  = mkModuleName "Data.UnitsOfMeasure.Internal"
    myPackage = fsLit "uom-plugin"


-- | Produce bogus evidence that the two types are equal.  Obviously
-- this will cause terrible trouble if they are in fact apart.
--
-- Previously we used a 'CoAxiomRule' for this, but that could not be
-- deserialized from interface files because 'tcIfaceCoAxiomRule' has
-- a built-in list of known CoAxiomRules.  Fortunately we can now
-- embed a 'UnivCo' in 'TcCoercion'.  In the future, we may want to
-- make it possible for plugins to create their own CoAxiomRules,
-- carring some sort of serializable code pointer instead of
-- 'coaxrProves'.
evByFiat :: String -> (Type, Type) -> EvTerm
evByFiat name (t1,t2) = EvCoercion $ TcCoercion $ mkUnivCo (fsLit name) Nominal t1 t2

-- | Produce bogus evidence for a constraint, including actual
-- equality constraints and our fake '(~~)' equality constraints.
evMagic :: UnitDefs -> Ct -> EvTerm
evMagic uds ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2   -> evByFiat "units" (t1, t2)
    IrredPred t
      | Just (tc, [t1,t2]) <- splitTyConApp_maybe t
      , tc == equivTyCon uds -> evByFiat "units" (t1, t2) `EvCast`
                                  TcCoercion (mkUnivCo (fsLit "units") Representational (mkTyConApp eqTyCon [typeKind t1, t1, t2]) t)
    _                    -> error "evMagic"
