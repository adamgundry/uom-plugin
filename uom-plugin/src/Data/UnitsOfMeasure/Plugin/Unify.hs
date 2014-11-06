module Data.UnitsOfMeasure.Plugin.Unify
  ( TySubst
  , SubstItem(..)
  , substsUnit
  , substsSubst
  , UnifyResult(..)
  , unifyUnits
  ) where

import Outputable
import TcRnTypes
import Type
import TypeRep

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
unifyUnits :: UnitDefs -> Ct -> NormUnit -> NormUnit -> TcPluginM UnifyResult
unifyUnits uds ct u0 v0 = do tcPluginTrace "unifyUnits" (ppr u0 $$ ppr v0)
                             unifyOne uds ct [] [] (u0 /: v0)

unifyOne :: UnitDefs -> Ct -> [TyVar] -> TySubst -> NormUnit -> TcPluginM UnifyResult
unifyOne uds ct tvs subst u
      | isOne u           = return $ Win tvs subst
      | isConstant u      = return   Lose
      | otherwise         = tcPluginTrace "unifyOne" (ppr u) >> go [] (ascending u)

      where
        go :: [(Atom, Integer)] -> [(Atom, Integer)] -> TcPluginM UnifyResult
        go _  []                       = return $ Draw tvs subst
        go ls (at@(VarAtom a, i) : xs) =
            case () of
                () | divisible i u -> let r = divideExponents (-i) $ leftover a u
                                             in return $ Win tvs $ SubstItem a r ct : subst
                   | any (not . isBase . fst) xs -> do TyVarTy beta <- newFlexiTyVarTy $ unitKind uds
                                                       let r = varUnit beta *: divideExponents (-i) (leftover a u)
                                                       unifyOne uds ct (beta:tvs) (SubstItem a r ct:subst) $ substUnit a r u
                   | otherwise            -> go (at:ls) xs

        go ls (at@(FamAtom f tys, i) : xs) = do
          mb <- matchFam f tys
          case normaliseUnit uds . snd =<< mb of
            Just v  -> unifyOne uds ct tvs subst $ mkNormUnit (ls ++ xs) *: v ^: i
            Nothing -> go (at:ls) xs
        go ls (at@(BaseAtom  _, _) : xs) = go (at:ls) xs
