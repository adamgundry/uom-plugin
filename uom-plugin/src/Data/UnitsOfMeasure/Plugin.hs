module Data.UnitsOfMeasure.Plugin
  ( tcPlugin
  ) where

import TcEvidence
import TcRnTypes
import TcType

import DataCon
import Type
import TyCon
import TypeRep
import TysWiredIn
import TysPrim
import PrelNames

import Bag
import FastString
import Panic ( panic )
import Outputable
import VarEnv
import Util ( thenCmp )

import SrcLoc ( noSrcSpan )
import TcMType ( zonkCt, newFlatWanted )
import UniqFM ( emptyUFM, UniqFM, foldUFM )
import TrieMap ( foldTM, ListMap, TypeMap )
import CoAxiom ( CoAxiomRule(..) )
import Pair ( Pair(..) )
import OccName ( occName, occNameFS, mkTcOcc )
import RdrName
import Module

import Control.Applicative
import Control.Monad
import Data.Either
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import Data.List ( sortBy )
import Data.Maybe
import Data.Ord

import Data.UnitsOfMeasure.Plugin.NormalForm
import TcPluginExtras


tcPlugin :: TcPlugin
tcPlugin = tracePlugin "uom-plugin" $ TcPlugin { tcPluginInit  = const $ lookupUnitDefs
                                               , tcPluginSolve = unitsOfMeasureSolver
                                               , tcPluginStop  = const $ return ()
                                               }


isUnitKind :: UnitDefs -> Type -> Bool
isUnitKind uds ty | Just (tc, _) <- tcSplitTyConApp_maybe ty = tc == unitKindCon uds
                  | otherwise                                = False


unitsOfMeasureSolver :: UnitDefs -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
unitsOfMeasureSolver uds givens deriveds wanteds = do
    (unit_givens , other_givens )  <- partitionEithers <$> mapM (toUnitEquality uds) givens
    (unit_wanteds, other_wanteds)  <- partitionEithers <$> mapM (toUnitEquality uds) wanteds
    let unit_cts = unit_givens ++ unit_wanteds
    case filter (not . isCFunEqCan . fromUnitEquality) unit_wanteds of
      []    -> return $ TcPluginOk [] []
      (_:_) -> do
        sr <- simplifyUnits uds unit_cts
        tcPluginTrace "unitsOfMeasureSolver simplified" (ppr sr)
        case sr of
          Simplified tvs subst evs eqs -> TcPluginOk (filter solvable evs)
                                            <$> mapM (mkWanted uds) (filter substable subst)
          Impossible (ct, u, v) eqs    -> return $ TcPluginContradiction [ct]
  where
    -- Extract the unit equality constraints
    toUnitEquality :: UnitDefs -> Ct -> TcPluginM (Either UnitEquality Ct)
    toUnitEquality uds ct = do
        ct' <- unsafeTcPluginTcM $ zonkCt ct -- TODO suspcious
        return $ case classifyPredType $ ctEvPred $ ctEvidence ct' of
                          EqPred t1 t2 | isUnitKind uds (typeKind t1) || isUnitKind uds (typeKind t1)
                                       , Just u1 <- normaliseUnit uds t1
                                       , Just u2 <- normaliseUnit uds t2
                                       -> Left (ct, u1, u2)
                          _            -> Right ct

    fromUnitEquality :: UnitEquality -> Ct
    fromUnitEquality (ct, _, _) = ct

    mkWanted :: UnitDefs -> SubstItem -> TcPluginM Ct
    mkWanted uds si = case siFlavour si of
        Wanted -> unsafeTcPluginTcM $ newFlatWanted (ctLocOrigin (siLoc si)) pred
        Given  -> return $ mkNonCanonical $ CtGiven pred (evByFiat "units" (ty1, ty2)) (siLoc si)
      where
        pred = mkEqPred ty1 ty2
        ty1  = mkTyVarTy (siVar si)
        ty2  = reifyUnit uds (siUnit si)

    -- plugins may give back only solutions to non-CFunEqCan wanted constraints
    solvable (_, ct) = not (isCFunEqCan ct) && isWanted (ctEvidence ct)

    -- In what can only be described as a grotesque hack, we simply
    -- throw away our solutions for flatten tyvars; this isn't sound
    -- in general (because our solution may be more restrictive than
    -- the result of unflattening)!
    substable si = not $ isFlattenTyVar $ siVar si


type UnitEquality = (Ct, NormUnit, NormUnit)

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
        ur <- unifyUnits uds (ctFlavour ct) (ctLoc ct) (substsUnit subst u) (substsUnit subst v)
        tcPluginTrace "unifyUnits result" (ppr ur)
        case ur of
          Win tvs' subst'  -> simples (tvs++tvs') (substsSubst subst' subst ++ subst')
                                  ((evByFiat "units" (getEqPredTys $ ctPred ct), ct):evs) [] (xs ++ eqs)
          Lose             -> return $ Impossible eq (xs ++ eqs)
          Draw [] []       -> simples tvs subst evs (eq:xs) eqs
          Draw tvs' subst' -> simples (tvs++tvs') (substsSubst subst' subst ++ subst') evs [eq] (xs ++ eqs)


data UnifyResult = Win [TyVar] TySubst | Lose | Draw [TyVar] TySubst

instance Outputable UnifyResult where
  ppr (Win  tvs subst) = text "Win"  <+> ppr tvs <+> ppr subst
  ppr (Draw tvs subst) = text "Draw" <+> ppr tvs <+> ppr subst
  ppr Lose             = text "Lose"

type TySubst = [SubstItem]

data SubstItem = SubstItem { siVar     :: TyVar
                           , siUnit    :: NormUnit
                           , siFlavour :: Flavour
                           , siLoc     ::  CtLoc
                           }

instance Outputable SubstItem where
  ppr si = ppr (siVar si) <+> text " := " <+> ppr (siUnit si)

data Flavour = Given | Wanted
  deriving (Eq, Show)

instance Outputable Flavour where
  ppr = text . show

ctFlavour :: Ct -> Flavour
ctFlavour ct | isGiven (ctEvidence ct) = Given
             | otherwise               = Wanted

unifyUnits :: UnitDefs -> Flavour -> CtLoc -> NormUnit -> NormUnit -> TcPluginM UnifyResult
unifyUnits uds fl loc u0 v0 = do tcPluginTrace ("unifyUnits " ++ show fl) (ppr u0 $$ ppr v0)
                                 unifyOne uds fl loc [] [] (u0 /: v0)

unifyOne :: UnitDefs -> Flavour -> CtLoc -> [TyVar] -> TySubst -> NormUnit -> TcPluginM UnifyResult
unifyOne uds fl loc tvs subst u
      | isOne u           = return $ Win tvs subst
      | isConstant u      = return   Lose
      | otherwise         = tcPluginTrace "unifyOne" (ppr u) >> go [] (ascending u)

      where
        go :: [(Atom, Integer)] -> [(Atom, Integer)] -> TcPluginM UnifyResult
        go _  []                       = return $ Draw tvs subst
        go ls (at@(VarAtom a, i) : xs) =
            case () of
                () | divisible i u -> let r = divideExponents (-i) $ leftover a u
                                             in return $ Win tvs $ SubstItem a r fl loc : subst
                   | any (not . isBase . fst) xs -> do TyVarTy beta <- newFlexiTyVarTy $ unitKind uds
                                                       let r = atom (VarAtom beta) *: divideExponents (-i) (leftover a u)
                                                       unifyOne uds fl loc (beta:tvs) (SubstItem a r fl loc:subst) $ substUnit (a, i) r u
                   | otherwise            -> go (at:ls) xs

        go ls (at@(FamAtom f tys, i) : xs) = do
          mb <- matchFam f tys
          case mb of
            Just (co, ty)
              | Just u <- normaliseUnit uds ty -> unifyOne uds fl loc tvs subst $ invariant (Map.fromList $ ls ++ xs) *: u ^: i
              | otherwise                  -> error "help help help help" -- TODO
            Nothing                        -> go (at:ls) xs -- TODO: more we can do here?
        go ls (at@(BaseAtom  _, _) : xs) = go (at:ls) xs



substsUnit :: TySubst -> NormUnit -> NormUnit
substsUnit [] u = u
substsUnit (si:s) u = case Map.lookup (VarAtom (siVar si)) u of
                             Nothing -> substsUnit s u
                             Just i  -> substsUnit s (substUnit (siVar si, i) (siUnit si) u)

substsSubst :: TySubst -> TySubst -> TySubst
substsSubst s = map $ \ si -> si { siUnit = substsUnit s (siUnit si) }



normaliseUnit :: UnitDefs -> Type -> Maybe NormUnit
normaliseUnit uds ty | Just ty1 <- tcView ty = normaliseUnit uds ty1
normaliseUnit _   (TyVarTy v)                    = pure $ atom $ VarAtom v
normaliseUnit uds (TyConApp tc tys)
  | tc == promoteDataCon (unitOneDataCon uds)                 = pure Map.empty
  | tc == promoteDataCon (unitBaseDataCon uds), [x]    <- tys = atom . BaseAtom <$> isStrLitTy x
  | tc == mulTyCon uds,    [u, v] <- tys = (*:) <$> normaliseUnit uds u <*> normaliseUnit uds v
  | tc == divTyCon uds,    [u, v] <- tys = (/:) <$> normaliseUnit uds u <*> normaliseUnit uds v
  | tc == expTyCon uds,    [u, n] <- tys = (^:) <$> normaliseUnit uds u <*> isNumLitTy n
  | isFamilyTyCon tc                             = pure $ atom $ FamAtom tc tys
normaliseUnit _ ty = Nothing


reifyUnit :: UnitDefs -> NormUnit -> Type
reifyUnit uds u | null xs && null ys = oneTy
                | null ys            = foldr1 times xs
                | null xs            = oneTy `divide` foldr1 times ys
                | otherwise          = foldr1 times xs `divide` foldr1 times ys
  where
    (pos, neg) = Map.partition (> 0) u
    xs = map fromAtom $ Map.toList pos
    ys = map fromAtom $ Map.toList $ Map.map negate neg

    oneTy      = mkTyConApp (promoteDataCon $ unitOneDataCon uds) []
    times  x y = mkTyConApp (mulTyCon uds) [x, y]
    divide x y = mkTyConApp (divTyCon uds) [x, y]

    fromAtom (a, n) = pow n (reifyAtom uds a)
    pow 1 ty = ty
    pow n ty = mkTyConApp (expTyCon uds) [ty, mkNumLitTy n]

reifyAtom :: UnitDefs -> Atom -> Type
reifyAtom uds (BaseAtom s)    = mkTyConApp (promoteDataCon (unitBaseDataCon uds)) [mkStrLitTy s]
reifyAtom _   (VarAtom  v)    = mkTyVarTy  v
reifyAtom _   (FamAtom f tys) = mkTyConApp f tys


data UnitDefs = UnitDefs
    { unitKind        :: Kind
    , unitKindCon     :: TyCon
    , unitOneDataCon  :: DataCon
    , unitBaseDataCon :: DataCon
    , mulTyCon        :: TyCon
    , divTyCon        :: TyCon
    , expTyCon        :: TyCon
    }

lookupUnitDefs :: TcPluginM UnitDefs
lookupUnitDefs = do
    u <- look "Unit"
    m <- look "*:"
    d <- look "/:"
    e <- look "^:"
    return $ UnitDefs (TyConApp (promoteTyCon u) []) u (getDataCon u "One") (getDataCon u "Base") m d e
  where
    getDataCon u s = case [ dc | dc <- tyConDataCons u, occNameFS (occName (dataConName dc)) == fsLit s ] of
                       [d] -> d
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


instance Outputable Integer where
    ppr = integer
