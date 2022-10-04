{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

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

import GhcApi hiding (tcPluginTrace)
import GhcApi.Shim
    ( mkEqPred, mkFunnyEqEvidence
    )

import qualified GHC.Plugins as Plugins
import qualified GHC.TcPlugin.API as PluginAPI

import Data.Either

import Data.UnitsOfMeasure.Plugin.Convert
import Data.UnitsOfMeasure.Plugin.NormalForm
import Data.UnitsOfMeasure.Plugin.Unify

-- | The plugin that GHC will load when this module is used with the
-- @-fplugin@ option.
plugin :: Plugins.Plugin
plugin =
    Plugins.defaultPlugin
        { Plugins.tcPlugin = const $ Just $ PluginAPI.mkTcPlugin uomPlugin
        , Plugins.pluginRecompile = const $ pure Plugins.NoForceRecompile
        }

uomPlugin :: PluginAPI.TcPlugin
uomPlugin =
    PluginAPI.TcPlugin
        { PluginAPI.tcPluginInit    = lookupUnitDefs
        , PluginAPI.tcPluginSolve   = unitsOfMeasureSolver
        , PluginAPI.tcPluginRewrite = unitsOfMeasureRewrite
        , PluginAPI.tcPluginStop    = const $ return ()
        }


unitsOfMeasureSolver :: UnitDefs -> [Ct] -> [Ct] -> PluginAPI.TcPluginM PluginAPI.Solve PluginAPI.TcPluginSolveResult
unitsOfMeasureSolver uds givens []      = do
    zonked_cts <- mapM PluginAPI.zonkCt givens
    let (unit_givens , _) = partitionEithers $ zipWith foo givens $ map (toUnitEquality uds) zonked_cts
    case unit_givens of
      []    -> return $ PluginAPI.TcPluginOk [] []
      (_:_) -> do
        sr <- simplifyUnits uds $ map snd unit_givens
        PluginAPI.tcPluginTrace "unitsOfMeasureSolver simplified givens only" $ ppr sr
        case sr of
          -- Simplified tvs []    evs eqs -> TcPluginOk (map (solvedGiven . fst) unit_givens) []
          Simplified _    -> return $ PluginAPI.TcPluginOk [] []
          Impossible eq _ -> reportContradiction uds eq
  where
    foo :: Ct -> Either UnitEquality Ct -> Either (Ct, UnitEquality) Ct
    foo ct (Left x)    = Left (ct, x)
    foo _  (Right ct') = Right ct'

    -- solvedGiven ct = (ctEvTerm (ctEvidence ct), ct)


unitsOfMeasureSolver uds givens wanteds = do
    let (unit_wanteds, _) = partitionEithers $ map (toUnitEquality uds) wanteds
    case unit_wanteds of
      []    -> return $ PluginAPI.TcPluginOk [] []
      (_:_) -> do
        (unit_givens , _) <- partitionEithers . map (toUnitEquality uds) <$> mapM PluginAPI.zonkCt givens
        sr <- simplifyUnits uds unit_givens
        PluginAPI.tcPluginTrace "unitsOfMeasureSolver simplified givens" $ ppr sr
        case sr of
          Impossible eq _ -> reportContradiction uds eq
          Simplified ss   -> do sr' <- simplifyUnits uds $ map (substsUnitEquality (simplifySubst ss)) unit_wanteds
                                PluginAPI.tcPluginTrace "unitsOfMeasureSolver simplified wanteds" $ ppr sr'
                                case sr' of
                                  Impossible _eq _ -> return $ PluginAPI.TcPluginOk [] [] -- Don't report a contradiction, see #22
                                  Simplified ss'  -> PluginAPI.TcPluginOk [ (evMagic uds ct, ct) | eq <- simplifySolved ss', let ct = fromUnitEquality eq ]
                                                         <$> mapM (substItemToCt uds) (filter (isWanted . ctEvidence . siCt) (substsSubst (simplifyUnsubst ss) (simplifySubst ss')))


reportContradiction :: UnitDefs -> UnitEquality -> PluginAPI.TcPluginM PluginAPI.Solve PluginAPI.TcPluginSolveResult
reportContradiction uds eq = PluginAPI.TcPluginContradiction . pure <$> fromUnitEqualityForContradiction uds eq

-- See #22 for why we need this
fromUnitEqualityForContradiction :: UnitDefs -> UnitEquality -> PluginAPI.TcPluginM PluginAPI.Solve Ct
fromUnitEqualityForContradiction uds (UnitEquality ct u v) = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq _ _ -> return ct
    _ | isGivenCt ct -> PluginAPI.mkNonCanonical <$> PluginAPI.newGiven  (ctLoc ct) (mkEqPred u' v') (evTermToExpr (mkFunnyEqEvidence (ctPred ct) u' v'))
      | otherwise    -> PluginAPI.mkNonCanonical <$> PluginAPI.newWanted (ctLoc ct) (mkEqPred u' v')
  where
    u' = reifyUnit uds u
    v' = reifyUnit uds v


substItemToCt :: UnitDefs -> SubstItem -> PluginAPI.TcPluginM PluginAPI.Solve Ct
substItemToCt uds si
      | isGiven (ctEvidence ct) = PluginAPI.mkNonCanonical <$> PluginAPI.newGiven loc prd (evByFiatExpr "units" ty1 ty2)
      | otherwise               = PluginAPI.mkNonCanonical <$> PluginAPI.newWanted loc prd
      where
        prd  = mkEqPred ty1 ty2
        ty1  = mkTyVarTy (siVar si)
        ty2  = reifyUnit uds (siUnit si)
        ct   = siCt si
        loc  = ctLoc ct


{-
TODO: this leads to errors like this on GHC 9.2, but seems to work on 9.4?

*** Core Lint errors : in result of Desugar (before optimization) ***
src/Data/UnitsOfMeasure/Defs.hs:19:4: warning:
    Trans coercion mis-match: (IsCanonical
                                 Univ(nominal plugin "units"
                                      :: Unpack (Base "m"), '["m"] ':/ '[]))_N
                              ; Sym (D:R:IsCanonical[0] <'["m"]>_N <'[]>_N)
      IsCanonical (Unpack (Base "m")) ~ IsCanonical ('["m"] ':/ '[])
      (AllIsCanonical '["m"], AllIsCanonical '[]) ~ IsCanonical
                                                      ('["m"] ':/ '[])
    In the RHS of $cp1HasCanonicalBaseUnit_alno :: IsCanonical
                                                     (Unpack (CanonicalBaseUnit "m"))
    In the body of letrec with binders $d(%%)_alnP :: () :: Constraint
    In the body of letrec with binders $d(%%)_alnN :: () :: Constraint
    In the body of letrec with binders $d~_alnO :: Base "m" ~ Base "m"
    In the body of letrec with binders $d(%,%)_alnM :: (Base "m"
                                                        ~ Base "m",
                                                        () :: Constraint)
    In the body of letrec with binders $d(%,%)_alnL :: ((Base "m"
                                                         ~ Base "m",
                                                         () :: Constraint),
                                                        () :: Constraint)
    Substitution: [TCvSubst
                     In scope: InScope {}
                     Type env: []
                     Co env: []]
-}

unitsOfMeasureRewrite
  :: UnitDefs ->
    PluginAPI.UniqFM
        TyCon
        ([Ct] -> [Type] -> PluginAPI.TcPluginM PluginAPI.Rewrite PluginAPI.TcPluginRewriteResult)
unitsOfMeasureRewrite uds = PluginAPI.listToUFM [(unpackTyCon uds, unpackRewriter uds)]

unpackRewriter :: UnitDefs -> [Ct] -> [Type] -> PluginAPI.TcPluginM PluginAPI.Rewrite PluginAPI.TcPluginRewriteResult
unpackRewriter uds _givens [ty] = do
  case maybeConstant =<< normaliseUnit uds ty of
    Nothing -> do PluginAPI.tcPluginTrace "unpackRewriter: no rewrite" (ppr ty)
                  pure PluginAPI.TcPluginNoRewrite
    Just u  -> do PluginAPI.tcPluginTrace "unpackRewriter: rewrite" (ppr ty <+> ppr u)
                  pure $ let reduct = reifyUnitUnpacked uds u
                         in let co = PluginAPI.mkPluginUnivCo "units" Nominal (mkTyConApp (unpackTyCon uds) [ty]) reduct
                            in PluginAPI.TcPluginRewriteTo (PluginAPI.Reduction co reduct) []
unpackRewriter _ _ tys = do
    PluginAPI.tcPluginTrace "unpackRewriter: wrong number of arguments?" (ppr tys)
    pure PluginAPI.TcPluginNoRewrite

-- TODO: the following is nonsense
lookupModule' :: PluginAPI.MonadTcPlugin m => PluginAPI.ModuleName -> p -> m PluginAPI.Module
lookupModule' modname _pkg = do
  r <- PluginAPI.findImportedModule modname PluginAPI.NoPkgQual --  (PluginAPI.OtherPkg pkg)
  case r of
    PluginAPI.Found _ md -> pure md
    _ -> do r' <- PluginAPI.findImportedModule modname PluginAPI.NoPkgQual
            case r' of
              PluginAPI.Found _ md -> pure md
              _ -> error "lookupModule: not Found"


lookupUnitDefs :: PluginAPI.TcPluginM PluginAPI.Init UnitDefs
lookupUnitDefs = do
    md <- lookupModule' myModule myPackage
    u <- look md "Unit"
    b <- look md "Base"
    o <- look md "One"
    m <- look md "*:"
    d <- look md "/:"
    e <- look md "^:"
    x <- look md "Unpack"
    i <- look md "UnitSyntax"
    c <- look md "~~"
    return $ UnitDefs u b o m d e x i (getDataCon i ":/") c
  where
    getDataCon u s = case [ dc | dc <- tyConDataCons u, occNameFS (occName (dataConName dc)) == fsLit s ] of
                       [d] -> promoteDataCon d
                       _   -> error $ "lookupUnitDefs/getDataCon: missing " ++ s

    look md s = PluginAPI.tcLookupTyCon =<< PluginAPI.lookupOrig md (mkTcOcc s)
    myModule  = mkModuleName "Data.UnitsOfMeasure.Internal"
    myPackage = fsLit "uom-plugin"


-- | Produce bogus evidence for a constraint, including actual
-- equality constraints and our fake '(~~)' equality constraints.
evMagic :: UnitDefs -> Ct -> EvTerm
evMagic uds ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2   -> evByFiat "units" t1 t2
    IrredPred t
      | Just (tc, [t1,t2]) <- splitTyConApp_maybe t
      , tc == equivTyCon uds -> mkFunnyEqEvidence t t1 t2
    _                    -> error "evMagic"

evByFiatExpr :: String -> PluginAPI.TcType -> PluginAPI.TcType -> EvExpr
evByFiatExpr s t1 t2 = evTermToExpr $ PluginAPI.mkPluginUnivEvTerm s Nominal t1 t2

evTermToExpr :: EvTerm -> EvExpr
evTermToExpr (EvExpr e) = e
evTermToExpr _ = error "evTermToExpr"
