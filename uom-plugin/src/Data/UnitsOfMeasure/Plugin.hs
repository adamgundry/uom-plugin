module Data.UnitsOfMeasure.Plugin
  ( plugin
  ) where

import Plugins

import TcEvidence
import TcRnTypes
import TcType

import DataCon
import Type
import TyCon
import TypeRep

import FastString
import Outputable

import TcMType ( newFlatWanted )
import CoAxiom ( CoAxiomRule(..) )
import Pair ( Pair(..) )
import OccName ( occName, occNameFS, mkTcOcc )
import RdrName
import Module

import Control.Applicative
import Data.Either
import qualified Data.Map as Map

import Data.UnitsOfMeasure.Plugin.Convert
import Data.UnitsOfMeasure.Plugin.NormalForm
import Data.UnitsOfMeasure.Plugin.Unify
import TcPluginExtras


plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const $ Just uomPlugin }

uomPlugin :: TcPlugin
uomPlugin = tracePlugin "uom-plugin" $ TcPlugin { tcPluginInit  = lookupUnitDefs
                                               , tcPluginSolve = unitsOfMeasureSolver
                                               , tcPluginStop  = const $ return ()
                                               }


unitsOfMeasureSolver :: UnitDefs -> Bool -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
unitsOfMeasureSolver uds is_final givens deriveds wanteds
  | is_final     = solverFinal uds givens wanteds
  | otherwise    = return $ TcPluginOk [] []

solverFinal :: UnitDefs -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solverFinal uds givens wanteds = do
    let (unit_wanteds, _) = partitionEithers $ map (toUnitEquality uds) wanteds
    case unit_wanteds of
      []    -> return $ TcPluginOk [] []
      (_:_) -> do
        (unit_givens , _) <- partitionEithers . map (toUnitEquality uds) <$> mapM zonkCt givens
        sr <- simplifyUnits uds $ unit_givens ++ unit_wanteds
        tcPluginTrace "unitsOfMeasureSolver simplified" (ppr sr)
        case sr of
          Simplified tvs subst evs eqs -> TcPluginOk (filter (isWanted . ctEvidence . snd) evs)
                                              <$> mapM (substItemToCt uds) (filter (isWanted . ctEvidence . siCt) subst)
          Impossible (ct, u, v) eqs    -> return $ TcPluginContradiction [ct]

substItemToCt :: UnitDefs -> SubstItem -> TcPluginM Ct
substItemToCt uds si
      | isGiven (ctEvidence ct) = return $ mkNonCanonical $ CtGiven pred (evByFiat "units" (ty1, ty2)) loc
      | otherwise               = unsafeTcPluginTcM $ newFlatWanted (ctLocOrigin loc) pred
      where
        pred = mkEqPred ty1 ty2
        ty1  = mkTyVarTy (siVar si)
        ty2  = reifyUnit uds (siUnit si)
        ct   = siCt si
        loc  = ctLoc ct


type UnitEquality = (Ct, NormUnit, NormUnit)

-- Extract the unit equality constraints
toUnitEquality :: UnitDefs -> Ct -> Either UnitEquality Ct
toUnitEquality uds ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
                      EqPred t1 t2 | isUnitKind uds (typeKind t1) || isUnitKind uds (typeKind t1)
                                   , Just u1 <- normaliseUnit uds t1
                                   , Just u2 <- normaliseUnit uds t2
                                   -> Left (ct, u1, u2)
                      _            -> Right ct

fromUnitEquality :: UnitEquality -> Ct
fromUnitEquality (ct, _, _) = ct


data SimplifyResult = Simplified [TyVar] TySubst [(EvTerm,Ct)] [UnitEquality] | Impossible UnitEquality [UnitEquality]

instance Outputable SimplifyResult where
  ppr (Simplified tvs subst evs eqs) = text "Simplified" $$ ppr tvs $$ ppr subst $$ ppr evs $$ ppr eqs
  ppr (Impossible eq eqs)            = text "Impossible" <+> ppr eq <+> ppr eqs

simplifyUnits :: UnitDefs -> [UnitEquality] -> TcPluginM SimplifyResult
simplifyUnits uds eqs = tcPluginTrace "simplifyUnits" (ppr eqs) >> simples [] [] [] [] eqs
  where
    simples :: [TyVar] -> TySubst -> [(EvTerm, Ct)] -> [UnitEquality] -> [UnitEquality] -> TcPluginM SimplifyResult
    simples tvs subst evs xs [] = return $ Simplified tvs subst evs xs
    simples tvs subst evs xs (eq@(ct, u, v):eqs) = do
        ur <- unifyUnits uds ct (substsUnit subst u) (substsUnit subst v)
        tcPluginTrace "unifyUnits result" (ppr ur)
        case ur of
          Win tvs' subst'  -> simples (tvs++tvs') (substsSubst subst' subst ++ subst')
                                  ((evByFiat "units" (getEqPredTys $ ctPred ct), ct):evs) [] (xs ++ eqs)
          Lose             -> return $ Impossible eq (xs ++ eqs)
          Draw [] []       -> simples tvs subst evs (eq:xs) eqs
          Draw tvs' subst' -> simples (tvs++tvs') (substsSubst subst' subst ++ subst') evs [eq] (xs ++ eqs)



lookupUnitDefs :: TcPluginM UnitDefs
lookupUnitDefs = do
    u <- look "Unit"
    m <- look "*:"
    d <- look "/:"
    e <- look "^:"
    return $ UnitDefs u (getDataCon u "One") (getDataCon u "Base") m d e
  where
    getDataCon u s = case [ dc | dc <- tyConDataCons u, occNameFS (occName (dataConName dc)) == fsLit s ] of
                       [d] -> promoteDataCon d
                       _   -> error $ "lookupUnitDefs/getDataCon: missing " ++ s

    look s = tcLookupTyCon =<< lookupRdrName myModule (mkRdrUnqual (mkTcOcc s))
    myModule = mkModuleName "Data.UnitsOfMeasure"


byFiat :: String -> CoAxiomRule
byFiat name =
    CoAxiomRule
        { coaxrName      = fsLit name
        , coaxrTypeArity = 2
        , coaxrAsmpRoles = []
        , coaxrRole      = Nominal
        , coaxrProves    = \ ts cs -> case (ts,cs) of
                                        ([s,t],[]) -> return (Pair s t)
                                        _          -> Nothing
        }

evByFiat :: String -> (Type, Type) -> EvTerm
evByFiat name (t1,t2) = EvCoercion $ mkTcAxiomRuleCo (byFiat name) [t1,t2] []
