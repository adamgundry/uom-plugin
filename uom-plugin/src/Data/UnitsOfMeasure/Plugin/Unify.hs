module Data.UnitsOfMeasure.Plugin.Unify
  ( SubstItem(..)
  , substsSubst
  , substsUnitEquality
  , UnitEquality(..)
  , toUnitEquality
  , fromUnitEquality
  , SimplifyState(..)
  , SimplifyResult(..)
  , simplifyUnits
  ) where

import GhcApi
import Data.UnitsOfMeasure.Plugin.Convert
import Data.UnitsOfMeasure.Plugin.NormalForm


-- | A substitution is essentially a list of (variable, unit) pairs,
-- but we keep the original 'Ct' that lead to the substitution being
-- made, for use when turning the substitution back into constraints.
type TySubst = [SubstItem]

data SubstItem = SubstItem { siVar     :: TyVar
                           , siUnit    :: NormUnit
                           , siCt     ::  Ct
                           }

instance Outputable SubstItem where
  ppr si = ppr (siVar si) <+> text " := " <+> ppr (siUnit si) <+> text "  {" <+> ppr (siCt si) <+> text "}"

-- | Apply a substitution to a single normalised unit
substsUnit :: NormUnit -> TySubst -> NormUnit
substsUnit = foldl (\ u si -> substUnit (siVar si) (siUnit si) u)

-- | Compose two substitutions
substsSubst :: TySubst -> TySubst -> TySubst
substsSubst s = map $ \ si -> si { siUnit = substsUnit (siUnit si) s }

substsUnitEquality :: TySubst -> UnitEquality -> UnitEquality
substsUnitEquality s (UnitEquality ct u v) = UnitEquality ct (substsUnit u s) (substsUnit v s)

extendSubst :: SubstItem -> TySubst -> TySubst
extendSubst si s = si : substsSubst [si] s


-- | Possible results of unifying a single pair of units.  In the
-- non-failing cases, we return a substitution and a list of fresh
-- variables that were created.
data UnifyResult = Win [TyVar] TySubst TySubst
                 | Draw [TyVar] TySubst TySubst
                 | Lose

instance Outputable UnifyResult where
  ppr (Win  tvs subst unsubst) = text "Win"  <+> ppr tvs <+> ppr subst <+> ppr unsubst
  ppr (Draw tvs subst unsubst) = text "Draw" <+> ppr tvs <+> ppr subst <+> ppr unsubst
  ppr Lose                     = text "Lose"


-- | Attempt to unify two normalised units to produce a unifying
-- substitution.  The 'Ct' is the equality between the non-normalised
-- (and perhaps less substituted) unit type expressions.
unifyUnits :: UnitDefs -> UnitEquality -> TcPluginM UnifyResult
unifyUnits uds (UnitEquality ct u0 v0) = do tcPluginTrace "unifyUnits" (ppr u0 $$ ppr v0)
                                            unifyOne uds ct [] [] [] (u0 /: v0)

unifyOne :: UnitDefs -> Ct -> [TyVar] -> TySubst -> TySubst -> NormUnit -> TcPluginM UnifyResult
unifyOne uds ct tvs subst unsubst u
      | isOne u           = return $ Win tvs subst unsubst
      | isConstant u      = return   Lose
      | otherwise         = tcPluginTrace "unifyOne" (ppr u) >> go [] (ascending u)

      where
        go :: [(Atom, Integer)] -> [(Atom, Integer)] -> TcPluginM UnifyResult
        go _  []                       = return $ Draw tvs subst unsubst
        go ls (at@(VarAtom a, i) : xs) = do
            tch <- if given_mode then return True else isTouchableTcPluginM a
            let r = divideExponents (-i) $ leftover a u
            case () of
                () | tch && divisible i u -> return $ if occurs a r then Draw tvs subst unsubst
                                                                    else Win tvs (extendSubst (SubstItem a r ct) subst) unsubst
                   | tch && any (not . isBase . fst) xs -> do beta <- newUnitVar
                                                              let subst'   = extendSubst (SubstItem a    (varUnit beta *: r) ct) subst
                                                                  unsubst' = extendSubst (SubstItem beta (varUnit a    /: r) ct) unsubst
                                                              unifyOne uds ct (beta:tvs) subst' unsubst' $ substUnit a (varUnit beta *: r) u
                   | otherwise            -> go (at:ls) xs

        go ls (at@(FamAtom f tys, i) : xs) = do
          mb <- matchFam f tys
          case normaliseUnit uds . snd =<< mb of
            Just v  -> unifyOne uds ct tvs subst unsubst $ mkNormUnit (ls ++ xs) *: v ^: i
            Nothing -> go (at:ls) xs
        go ls (at@(BaseAtom  _, _) : xs) = go (at:ls) xs


        given_mode = isGiven (ctEvidence ct)

        newUnitVar | given_mode = newSkolemTyVar $ unitKind uds
                   | otherwise  = newFlexiTyVar  $ unitKind uds

        newSkolemTyVar kind = do
            x <- newUnique
            let name = mkSysTvName x (fsLit "beta")
            return $ mkTcTyVar name kind vanillaSkolemTv


data UnitEquality = UnitEquality Ct NormUnit NormUnit

instance Outputable UnitEquality where
  ppr (UnitEquality ct u v) = text "UnitEquality" $$ ppr ct $$ ppr u $$ ppr v

-- Extract the unit equality constraints
toUnitEquality :: UnitDefs -> Ct -> Either UnitEquality Ct
toUnitEquality uds ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2
      | isUnitKind uds (typeKind t1) || isUnitKind uds (typeKind t1)
      , Just u1 <- normaliseUnit uds t1
      , Just u2 <- normaliseUnit uds t2 -> Left (UnitEquality ct u1 u2)
    IrredPred t
      | Just (tc, [t1,t2]) <- splitTyConApp_maybe t
      , tc == equivTyCon uds
      , Just u1 <- normaliseUnit uds t1
      , Just u2 <- normaliseUnit uds t2 -> Left (UnitEquality ct u1 u2)
    _                                   -> Right ct

fromUnitEquality :: UnitEquality -> Ct
fromUnitEquality (UnitEquality ct _ _) = ct


data SimplifyState
  = SimplifyState { simplifyFreshVars :: [TyVar]
                  , simplifySubst     :: TySubst
                  , simplifyUnsubst   :: TySubst
                  , simplifySolved    :: [UnitEquality]
                  , simplifyStuck     :: [UnitEquality]
                  }

instance Outputable SimplifyState where
  ppr ss = text "fresh   = " <+> ppr (simplifyFreshVars ss)
        $$ text "subst   = " <+> ppr (simplifySubst     ss)
        $$ text "unsubst = " <+> ppr (simplifyUnsubst   ss)
        $$ text "solved  = " <+> ppr (simplifySolved    ss)
        $$ text "stuck   = " <+> ppr (simplifyStuck     ss)

initialState :: SimplifyState
initialState = SimplifyState [] [] [] [] []

data SimplifyResult
  = Simplified SimplifyState
  | Impossible { simplifyImpossible :: UnitEquality
               , simplifyRemaining  :: [UnitEquality]
               }

instance Outputable SimplifyResult where
  ppr (Simplified ss)     = text "Simplified" $$ ppr ss
  ppr (Impossible eq eqs) = text "Impossible" <+> ppr eq <+> ppr eqs

simplifyUnits :: UnitDefs -> [UnitEquality] -> TcPluginM SimplifyResult
simplifyUnits uds eqs0 = tcPluginTrace "simplifyUnits" (ppr eqs0) >> simples initialState eqs0
  where
    simples :: SimplifyState -> [UnitEquality] -> TcPluginM SimplifyResult
    simples ss [] = return $ Simplified ss
    simples ss (eq:eqs) = do
        ur <- unifyUnits uds (substsUnitEquality (simplifySubst ss) eq)
        tcPluginTrace "unifyUnits result" (ppr ur)
        case ur of
          Win  tvs subst unsubst -> let (ss', xs) = win eq tvs subst unsubst ss
                                    in simples ss' (xs ++ eqs)
          Draw _   []    _       -> simples (addStuck eq ss) eqs
          Draw tvs subst unsubst -> let (ss', xs) = draw eq tvs subst unsubst ss
                                    in simples ss' (xs ++ eqs)
          Lose                   -> return Impossible { simplifyImpossible = eq
                                                      , simplifyRemaining  = simplifyStuck ss ++ eqs }

win :: UnitEquality -> [TyVar] -> TySubst -> TySubst -> SimplifyState -> (SimplifyState, [UnitEquality])
win eq tvs subst unsubst ss =
  ( SimplifyState { simplifyFreshVars = simplifyFreshVars ss ++ tvs
                  , simplifySubst     = substsSubst subst (simplifySubst ss) ++ subst
                  , simplifyUnsubst   = substsSubst unsubst (simplifyUnsubst ss) ++ unsubst
                  , simplifySolved    = eq : simplifySolved ss
                  , simplifyStuck     = []
                  }
  , simplifyStuck ss )

draw :: UnitEquality -> [TyVar] -> TySubst -> TySubst -> SimplifyState -> (SimplifyState, [UnitEquality])
draw eq tvs subst unsubst ss =
  ( SimplifyState { simplifyFreshVars = simplifyFreshVars ss ++ tvs
                  , simplifySubst     = substsSubst subst (simplifySubst ss) ++ subst
                  , simplifyUnsubst   = substsSubst unsubst (simplifyUnsubst ss) ++ unsubst
                  , simplifySolved    = simplifySolved ss
                  , simplifyStuck     = [eq]
                  }
  , simplifyStuck ss )

addStuck :: UnitEquality -> SimplifyState -> SimplifyState
addStuck eq ss = ss { simplifyStuck = eq : simplifyStuck ss }
