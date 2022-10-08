{-# LANGUAGE DataKinds #-}

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

import GhcApi

import qualified GHC.Plugins as Plugins
import GHC.TcPlugin.API as PluginAPI

import Data.Either ( partitionEithers )

import Data.UnitsOfMeasure.Plugin.Convert
import Data.UnitsOfMeasure.Plugin.Lookup
import Data.UnitsOfMeasure.Plugin.NormalForm
import Data.UnitsOfMeasure.Plugin.Unify

-- | The plugin that GHC will load when this module is used with the
-- @-fplugin@ option.
plugin :: Plugins.Plugin
plugin =
    Plugins.defaultPlugin
        { Plugins.tcPlugin = const $ Just $ PluginAPI.mkTcPlugin uomPlugin
        , Plugins.pluginRecompile = Plugins.purePlugin
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
    PluginAPI.tcPluginTrace "[UOM] unitsOfMeasureSolver simplifying givens" $ ppr givens
    let (unit_givens0 , _) = partitionEithers $ zipWith foo givens $ map (toUnitEquality uds) givens
    let unit_givens = filter is_useful unit_givens0
    case unit_givens of
      []    -> return $ PluginAPI.TcPluginOk [] []
      (_:_) -> do
        sr <- simplifyUnits uds $ map snd unit_givens
        PluginAPI.tcPluginTrace "[UOM] unitsOfMeasureSolver simplified givens only" $ ppr sr
        case sr of
          -- TODO: givens simplification is currently disabled, because if we emit a given
          -- constraint like x[sk] ~ Base "kg" then GHC will "simplify" all occurrences
          -- of the type family application Base "kg" to the skolem variable x[sk].
          -- This can then result in loops as the rewriter will turn the fam app into
          -- the variable, then the plugin will "solve" it again.
          Simplified _ -> pure $ PluginAPI.TcPluginOk [] []
          Simplified ss   -> do
              -- TODO: we ought to generate evidence that depends on the
              -- previous givens (and similarly when simplifying wanteds, the
              -- evidence we generate should depend on the new wanteds).
              -- Otherwise we could potentially have a soundness issue e.g. if a
              -- GADT pattern match brings a unit equality into scope, but we
              -- later float out something that depends on it.
              let usefuls = simplifySubst ss
              xs <- mapM (substItemToCt uds) usefuls
              pure $ PluginAPI.TcPluginOk (map (solvedGiven . siCt) usefuls) xs
          -- Simplified _    -> return $ PluginAPI.TcPluginOk [] []
          Impossible eq _ -> reportContradiction uds eq
  where
    foo :: Ct -> Either UnitEquality Ct -> Either (Ct, UnitEquality) Ct
    foo ct (Left x)    = Left (ct, x)
    foo _  (Right ct') = Right ct'

    solvedGiven ct = (ctEvTerm (ctEvidence ct), ct)

    -- TODO: if the simplify givens stage makes progress, we want to emit new
    -- givens in case GHC can substitute into constraints other than unit
    -- equalities.  However, we don't want to cause a loop by repeatedly
    -- re-simplifying the same givens.  We currently have a conservative check
    -- to see if it is useful to simplify a unit equality: if neither side of
    -- the original equality was a single variable.  There are "useful" cases
    -- this misses, however, e.g. v^2 ~ v.
    is_useful (_, ue) = isUsefulUnitEquality ue

unitsOfMeasureSolver uds givens wanteds = do
    let (unit_wanteds, _) = partitionEithers $ map (toUnitEquality uds) wanteds
    case unit_wanteds of
      []    -> return $ PluginAPI.TcPluginOk [] []
      (_:_) -> do
        let (unit_givens , _) = partitionEithers $ map (toUnitEquality uds) givens
        sr <- simplifyUnits uds unit_givens
        PluginAPI.tcPluginTrace "[UOM] unitsOfMeasureSolver simplified givens" $ ppr sr
        -- TODO: it is somewhat questionable to simplify the givens again
        -- here. In principle we should be able to simplify them at the
        -- simplify-givens stage, turn them into a substitution, and have GHC
        -- apply the substitution.
        case sr of
          Impossible eq _ -> reportContradiction uds eq
          Simplified ss   -> do sr' <- simplifyUnits uds $ map (substsUnitEquality (simplifySubst ss)) unit_wanteds
                                PluginAPI.tcPluginTrace "[UOM] unitsOfMeasureSolver simplified wanteds" $ ppr sr'
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
    _ | isGivenCt ct -> PluginAPI.mkNonCanonical <$> PluginAPI.newGiven  (ctLoc ct) (mkPrimEqPred u' v') (evTermToExpr (mkFunnyEqEvidence (ctPred ct) u' v'))
      | otherwise    -> PluginAPI.mkNonCanonical <$> PluginAPI.newWanted (ctLoc ct) (mkPrimEqPred u' v')
  where
    u' = reifyUnit uds u
    v' = reifyUnit uds v


substItemToCt :: UnitDefs -> SubstItem -> PluginAPI.TcPluginM PluginAPI.Solve Ct
substItemToCt uds si
      | isGiven (ctEvidence ct) = PluginAPI.mkNonCanonical <$> PluginAPI.newGiven loc prd (evByFiatExpr "units" ty1 ty2)
      | otherwise               = PluginAPI.mkNonCanonical <$> PluginAPI.newWanted loc prd
      where
        prd  = mkPrimEqPred ty1 ty2
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
    Nothing -> do PluginAPI.tcPluginTrace "[UOM] unpackRewriter: no rewrite" (ppr ty)
                  pure PluginAPI.TcPluginNoRewrite
    Just u  -> do PluginAPI.tcPluginTrace "[UOM] unpackRewriter: rewrite" (ppr ty <+> ppr u)
                  pure $ let reduct = reifyUnitUnpacked uds u
                         in let co = PluginAPI.mkPluginUnivCo "units" Nominal (mkTyConApp (unpackTyCon uds) [ty]) reduct
                            in PluginAPI.TcPluginRewriteTo (PluginAPI.Reduction co reduct) []
unpackRewriter _ _ tys = do
    PluginAPI.tcPluginTrace "[UOM] unpackRewriter: wrong number of arguments?" (ppr tys)
    pure PluginAPI.TcPluginNoRewrite


-- | Make up evidence for a fake equality constraint @t1 ~~ t2@ by coercing
-- bogus evidence of type @t1 ~ t2@.
mkFunnyEqEvidence :: Type -> Type -> Type -> EvTerm
mkFunnyEqEvidence t t1 t2 =
    castFrom `evCast'` castTo
    where
        castFrom :: EvTerm
        castFrom = evDFunApp funId tys terms
            where
                funId :: Id
                funId = dataConWrapId heqDataCon

                tys :: [Kind]
                tys = [typeKind t1, typeKind t2, t1, t2]

                terms :: [EvExpr]
                terms = [evByFiatExpr "units" t1 t2]

        castTo :: TcCoercion
        castTo =
            mkUnivCo from Representational tySource t
            where
                from :: UnivCoProvenance
                from = PluginProv "units"

                tySource :: Type
                tySource = mkHEqPred t1 t2

mkHEqPred :: Type -> Type -> Type
mkHEqPred t1 t2 = TyConApp heqTyCon [typeKind t1, typeKind t2, t1, t2]


-- | Produce bogus evidence for a constraint, including actual
-- equality constraints and our fake '(~~)' equality constraints.
evMagic :: UnitDefs -> Ct -> EvTerm
evMagic uds ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2   -> evByFiat "units" t1 t2
    IrredPred t
      | Just (tc, [t1,t2]) <- splitTyConApp_maybe t
      , tc == equivTyCon uds -> mkFunnyEqEvidence t t1 t2
    _                    -> error "evMagic"

evByFiat :: String -> PluginAPI.TcType -> PluginAPI.TcType -> EvTerm
evByFiat s t1 t2 = PluginAPI.mkPluginUnivEvTerm s Nominal t1 t2

evByFiatExpr :: String -> PluginAPI.TcType -> PluginAPI.TcType -> EvExpr
evByFiatExpr s t1 t2 = evTermToExpr $ PluginAPI.mkPluginUnivEvTerm s Nominal t1 t2

evTermToExpr :: EvTerm -> EvExpr
evTermToExpr (EvExpr e) = e
evTermToExpr _ = error "evTermToExpr"

evCast' :: EvTerm -> TcCoercion -> EvTerm
evCast' = evCast . evTermToExpr
