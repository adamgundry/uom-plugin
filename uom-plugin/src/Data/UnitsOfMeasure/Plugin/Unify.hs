module Data.UnitsOfMeasure.Plugin.Unify
  ( SubstItem(..)
  , UnitEquality
  , SimplifyState(..)
  , SimplifyResult(..)
  , simplifyUnits
  ) where

import FastString
import Name
import Outputable
import TcRnMonad ( Ct, isGiven, ctEvidence )
import TcType
import Type
import Var

import Data.UnitsOfMeasure.Plugin.Convert
import Data.UnitsOfMeasure.Plugin.NormalForm
import TcPluginExtras


-- | A substitution is essentially a list of (variable, unit) pairs,
-- but we keep the original 'Ct' that lead to the substitution being
-- made, for use when turning the substitution back into constraints.
type TySubst = [SubstItem]

data SubstItem = SubstItem { siVar     :: TyVar
                           , siUnit    :: NormUnit
                           , siCt     ::  Ct
                           }

instance Outputable SubstItem where
  ppr si = ppr (siVar si) <+> text " := " <+> ppr (siUnit si)

-- | Apply a substitution to a single normalised unit
substsUnit :: TySubst -> NormUnit -> NormUnit
substsUnit []     u = u
substsUnit (si:s) u = substsUnit s (substUnit (siVar si) (siUnit si) u)

-- | Compose two substitutions
substsSubst :: TySubst -> TySubst -> TySubst
substsSubst s = map $ \ si -> si { siUnit = substsUnit s (siUnit si) }

substsUnitEquality :: TySubst -> UnitEquality -> UnitEquality
substsUnitEquality s (ct, u, v) = (ct, substsUnit s u, substsUnit s v)

-- | Possible results of unifying a single pair of units.  In the
-- non-failing cases, we return a substitution and a list of fresh
-- variables that were created.
data UnifyResult = Win [TyVar] TySubst | Lose | Draw [TyVar] TySubst

instance Outputable UnifyResult where
  ppr (Win  tvs subst) = text "Win"  <+> ppr tvs <+> ppr subst
  ppr (Draw tvs subst) = text "Draw" <+> ppr tvs <+> ppr subst
  ppr Lose             = text "Lose"


-- | Attempt to unify two normalised units to produce a unifying
-- substitution.  The 'Ct' is the equality between the non-normalised
-- (and perhaps less substituted) unit type expressions.
unifyUnits :: UnitDefs -> UnitEquality -> TcPluginM UnifyResult
unifyUnits uds (ct, u0, v0) = do tcPluginTrace "unifyUnits" (ppr u0 $$ ppr v0)
                                 unifyOne uds ct [] [] (u0 /: v0)

unifyOne :: UnitDefs -> Ct -> [TyVar] -> TySubst -> NormUnit -> TcPluginM UnifyResult
unifyOne uds ct tvs subst u
      | isOne u           = return $ Win tvs subst
      | isConstant u      = return   Lose
      | otherwise         = tcPluginTrace "unifyOne" (ppr u) >> go [] (ascending u)

      where
        go :: [(Atom, Integer)] -> [(Atom, Integer)] -> TcPluginM UnifyResult
        go _  []                       = return $ Draw tvs subst
        go ls (at@(VarAtom a, i) : xs) = do
            tch <- if given_mode then return True else isTouchableTcPluginM a
            let r = divideExponents (-i) $ leftover a u
            case () of
                () | tch && divisible i u -> return $ if occurs a r then Draw tvs subst
                                                                    else Win tvs $ SubstItem a r ct : subst
                   | tch && any (not . isBase . fst) xs -> do beta <- newUnitVar
                                                              let r = varUnit beta *: divideExponents (-i) (leftover a u)
                                                              unifyOne uds ct (beta:tvs) (SubstItem a r ct:subst) $ substUnit a r u
                   | otherwise            -> go (at:ls) xs

        go ls (at@(FamAtom f tys, i) : xs) = do
          mb <- matchFam f tys
          case normaliseUnit uds . snd =<< mb of
            Just v  -> unifyOne uds ct tvs subst $ mkNormUnit (ls ++ xs) *: v ^: i
            Nothing -> go (at:ls) xs
        go ls (at@(BaseAtom  _, _) : xs) = go (at:ls) xs


        given_mode = isGiven (ctEvidence ct)

        newUnitVar | given_mode = newSkolemTyVar $ unitKind uds
                   | otherwise  = newFlexiTyVar  $ unitKind uds

        newSkolemTyVar kind = do
            uniq <- newUnique
            let name = mkSysTvName uniq (fsLit "beta")
            return $ mkTcTyVar name kind vanillaSkolemTv


type UnitEquality = (Ct, NormUnit, NormUnit)

data SimplifyState
  = SimplifyState { simplifyFreshVars :: [TyVar]
                  , simplifySubst     :: TySubst
                  , simplifySolved    :: [UnitEquality]
                  , simplifyStuck     :: [UnitEquality]
                  }

instance Outputable SimplifyState where
  ppr (SimplifyState fresh subst solved stuck)
    = text "fresh = " <+> ppr fresh
      $$ text "subst = " <+> ppr subst
      $$ text "solved = " <+> ppr solved
      $$ text "stuck = " <+> ppr stuck

initialState :: SimplifyState
initialState = SimplifyState [] [] [] []

data SimplifyResult
  = Simplified SimplifyState
  | Impossible { simplifyImpossible :: UnitEquality
               , simplifyRemaining  :: [UnitEquality]
               }

instance Outputable SimplifyResult where
  ppr (Simplified ss)     = text "Simplified" $$ ppr ss
  ppr (Impossible eq eqs) = text "Impossible" <+> ppr eq <+> ppr eqs

simplifyUnits :: UnitDefs -> [UnitEquality] -> TcPluginM SimplifyResult
simplifyUnits uds eqs = tcPluginTrace "simplifyUnits" (ppr eqs) >> simples initialState eqs
  where
    simples :: SimplifyState -> [UnitEquality] -> TcPluginM SimplifyResult
    simples ss [] = return $ Simplified ss
    simples ss (eq:eqs) = do
        ur <- unifyUnits uds (substsUnitEquality subst eq)
        tcPluginTrace "unifyUnits result" (ppr ur)
        case ur of
          Win tvs' subst'  -> let (ss', xs) = win eq tvs' subst' ss
                              in simples ss' (xs ++ eqs)
          Draw [] []       -> simples (addStuck eq ss) eqs
          Draw tvs' subst' -> let (ss', xs) = draw eq tvs' subst' ss
                              in simples ss' (xs ++ eqs)
          Lose             -> return Impossible { simplifyImpossible = eq
                                                , simplifyRemaining  = simplifyStuck ss ++ eqs }
      where
        subst = simplifySubst ss

win :: UnitEquality -> [TyVar] -> TySubst -> SimplifyState -> (SimplifyState, [UnitEquality])
win eq tvs subst ss =
  ( SimplifyState { simplifyFreshVars = simplifyFreshVars ss ++ tvs
                  , simplifySubst     = substsSubst subst (simplifySubst ss) ++ subst
                  , simplifySolved    = eq : simplifySolved ss
                  , simplifyStuck     = []
                  }
  , simplifyStuck ss )

draw :: UnitEquality -> [TyVar] -> TySubst -> SimplifyState -> (SimplifyState, [UnitEquality])
draw eq tvs subst ss =
  ( SimplifyState { simplifyFreshVars = simplifyFreshVars ss ++ tvs
                  , simplifySubst     = substsSubst subst (simplifySubst ss) ++ subst
                  , simplifySolved    = simplifySolved ss
                  , simplifyStuck     = [eq]
                  }
  , simplifyStuck ss )

addStuck :: UnitEquality -> SimplifyState -> SimplifyState
addStuck eq ss = ss { simplifyStuck = eq : simplifyStuck ss }
