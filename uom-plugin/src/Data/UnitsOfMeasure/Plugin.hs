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
import qualified GhcApi (tcPluginTrace)
import GhcApi.Shim
    ( mkEqPred, mkFunnyEqEvidence
    )

import qualified GHC.Plugins as Plugins
import qualified GHC.TcPlugin.API as PluginAPI
import qualified GHC.TcPlugin.API.Internal as PluginAPI.Internal

import qualified GHC.Tc.Utils.Monad as GHC (traceTc)

import Control.Applicative hiding ((<**>))
import Data.Either
import Data.Maybe

import Data.UnitsOfMeasure.Plugin.Convert
import Data.UnitsOfMeasure.Plugin.NormalForm
import Data.UnitsOfMeasure.Plugin.Unify

-- | The plugin that GHC will load when this module is used with the
-- @-fplugin@ option.
plugin :: Plugin
plugin = Plugins.defaultPlugin { Plugins.tcPlugin = const $ Just $ PluginAPI.mkTcPlugin uomPlugin
                               , Plugins.pluginRecompile = const $ pure NoForceRecompile
                               }

uomPlugin :: PluginAPI.TcPlugin
uomPlugin = PluginAPI.TcPlugin { PluginAPI.tcPluginInit  = lookupUnitDefs
                               , PluginAPI.tcPluginSolve = unitsOfMeasureSolver
                               , PluginAPI.tcPluginRewrite = \ uds -> PluginAPI.listToUFM [(unpackTyCon uds, unpackRewriter uds)]
                               , PluginAPI.tcPluginStop  = const $ return ()
                               }


unitsOfMeasureSolver :: UnitDefs -> [Ct] -> [Ct] -> PluginAPI.TcPluginM PluginAPI.Solve PluginAPI.TcPluginSolveResult
unitsOfMeasureSolver uds givens []      = do
    zonked_cts <- mapM PluginAPI.zonkCt givens
    let (unit_givens , _) = partitionEithers $ zipWith foo givens $ map (toUnitEquality uds) zonked_cts
    case unit_givens of
      []    -> return $ PluginAPI.TcPluginOk [] []
      (_:_) -> do
        sr <- simplifyUnits uds $ map snd unit_givens
        tcPluginTrace "unitsOfMeasureSolver simplified givens only" $ ppr sr
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
{-
  mb <- lookForUnpacks uds wanteds
  case mb of
   Just (new_cts, solved_cts) -> return $ TcPluginOk solved_cts new_cts
   Nothing -> do -}
    let (unit_wanteds, _) = partitionEithers $ map (toUnitEquality uds) wanteds
    case unit_wanteds of
      []    -> return $ PluginAPI.TcPluginOk [] []
      (_:_) -> do
        (unit_givens , _) <- partitionEithers . map (toUnitEquality uds) <$> mapM PluginAPI.zonkCt givens
        sr <- simplifyUnits uds unit_givens
        tcPluginTrace "unitsOfMeasureSolver simplified givens" $ ppr sr
        case sr of
          Impossible eq _ -> reportContradiction uds eq
          Simplified ss   -> do sr' <- simplifyUnits uds $ map (substsUnitEquality (simplifySubst ss)) unit_wanteds
                                tcPluginTrace "unitsOfMeasureSolver simplified wanteds" $ ppr sr'
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
-- | If there are any occurrences of Unpack that can be reduced, return the new
-- Cts and solved Cts.
--
-- Previously, when we found an Unpack redex we emitted a new Given Ct claiming
-- that it was equal to its reduct.  However this appears not to work any more.
-- So instead, we look for Unpack occurrences only in Wanteds, and solve the
-- entire Wanted with a new one that contains the reduct.  This means that
-- Unpack will not be reduced in Givens.
--
-- All this should become much simpler when plugins can define type family
-- reductions directly.
lookForUnpacks :: UnitDefs -> [Ct] -> TcPluginM (Maybe ([Ct], [(EvTerm, Ct)]))
lookForUnpacks uds = lookForReductions (reduceType match_unpack)
  where
    match_unpack tc as
      | [a] <- as, tc == unpackTyCon uds = reifyUnitUnpacked uds <$> (maybeConstant =<< normaliseUnit uds a)
      | otherwise                        = Nothing

-- | Look for constraints whose types can be simplified by the reduction
-- function.  If there are any, emit new wanteds and solve the existing wanteds
-- using the new ones (coerced by UnivCo).
lookForReductions :: (Type -> Maybe Type) -> [Ct] -> TcPluginM (Maybe ([Ct], [(EvTerm, Ct)]))
lookForReductions reduce cts
    | null simplified_cts = pure Nothing
    | otherwise           = Just . unzip <$> mapM simplifyWanted simplified_cts
  where
    simplified_cts :: [(Ct, Type)]
    simplified_cts = mapMaybe (\ct -> (ct ,) <$> reduce (ctEvPred (ctEvidence ct))) cts

-- | Given a wanted constraint and a simplified type, make a new wanted and
-- return it along with evidence for the old wanted.
simplifyWanted :: (Ct, Type) -> TcPluginM (Ct, (EvTerm, Ct))
simplifyWanted (ct, ty) = do
    new_ct <- PluginAPI.mkNonCanonical <$> PluginAPI.newWanted (ctLoc ct) ty
    let ev = evCast (ctEvExpr (ctEvidence new_ct))
                    (mkUnivCo (PluginProv "uom-plugin") Representational ty (ctEvPred (ctEvidence ct)))
    pure (new_ct, (ev, ct))
-}


{-
TODO: this leads to errors like this:

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
unpackRewriter :: UnitDefs -> [Ct] -> [Type] -> PluginAPI.TcPluginM PluginAPI.Rewrite PluginAPI.TcPluginRewriteResult
unpackRewriter uds _givens [ty] = do
  -- mb <- normaliseUnitRewrite uds ty
  case maybeConstant =<< normaliseUnit uds ty of
    Nothing -> do tcPluginTrace "unpackRewriter: no rewrite" (ppr ty)
                  pure $ PluginAPI.TcPluginNoRewrite
    Just u  -> do tcPluginTrace "unpackRewriter: rewrite" (ppr ty <+> ppr u)
                  pure $ let reduct = reifyUnitUnpacked uds u
                         in let co = PluginAPI.mkPluginUnivCo "units" Nominal (mkTyConApp (unpackTyCon uds) [ty]) reduct
                            in PluginAPI.TcPluginRewriteTo (PluginAPI.Reduction co reduct) []


-- | Try to convert a type to a unit normal form; this does not check
-- the type has kind 'Unit', and may fail even if it does.
normaliseUnitRewrite :: UnitDefs -> Type -> PluginAPI.TcPluginM PluginAPI.Rewrite (Maybe NormUnit)
normaliseUnitRewrite uds ty | Just ty1 <- coreView ty = normaliseUnitRewrite uds ty1
normaliseUnitRewrite _   (TyVarTy v)              = ppure $ varUnit v
normaliseUnitRewrite uds (TyConApp tc tys)
  | tc == unitOneTyCon  uds                = ppure one
  | tc == unitBaseTyCon uds, [x]    <- tys = ppure $ baseUnit x
  | tc == mulTyCon      uds, [u, v] <- tys = (*:) <$$> normaliseUnitRewrite uds u <**> normaliseUnitRewrite uds v
  | tc == divTyCon      uds, [u, v] <- tys = (/:) <$$> normaliseUnitRewrite uds u <**> normaliseUnitRewrite uds v
  | tc == expTyCon      uds, [u, n] <- tys, Just i <- isNumLitTy n = (^:) <$$> normaliseUnitRewrite uds u <**> ppure i
  | isFamilyTyCon tc                       = do
      mb <- PluginAPI.matchFam tc tys
      case mb of
        Nothing -> ppure $ famUnit tc tys
        Just (PluginAPI.Reduction co rty) -> normaliseUnitRewrite uds rty -- TODO don't discard coercion
normaliseUnitRewrite _ _ = pure Nothing


ppure = pure . pure

(<$$>) :: (Monad f, Monad g) => (a -> b) -> f (g a) -> f (g b)
mf <$$> mmx = do mx <- mmx
                 pure $ mf <$> mx

(<**>) :: (Monad f, Monad g) => f (g (a -> b)) -> f (g a) -> f (g b)
mmf <**> mmx = do mf <- mmf
                  mx <- mmx
                  pure $ mf <*> mx

-- | Search a type for an occurrence of Unpack applied to a constant.  Such
-- occurrences can be reduced to the corresponding syntactic representation of
-- the constant.  If one is found, return the corresponding type with the
-- reduction applied.
reduceType :: (TyCon -> [Type] -> Maybe Type) -> Type -> Maybe Type
reduceType reduce = go
  where
    go :: Type -> Maybe Type
    go (AppTy f s)       = uncurry mkAppTy <$> go2 f s
    go (TyConApp tc as)  = reduce tc as <|> (mkTyConApp tc <$> go_tys [] as)
    go (FunTy x y t1 t2) = uncurry (mkFunTy x y) <$> go2 t1 t2
    go _                 = Nothing

    go2 :: Type -> Type -> Maybe (Type, Type)
    go2 t1 t2 = ((, t2) <$> go t1) <|> ((t1 ,) <$> go t2)

    -- The first argument accumulates preceding arguments that cannot be reduced.
    --
    -- TODO: the following is almost certainly wrong if the TyCon has a
    -- dependent kind, because simplifying the Unpack argument will change the
    -- kind of subsequent arguments and/or the result!
    go_tys :: [Type] -> [Type] -> Maybe [Type]
    go_tys  _  [] = Nothing
    go_tys xs (y:ys) = case go y of
                         Just y' -> Just (reverse xs ++ y':ys)
                         Nothing -> go_tys (y:xs) ys


-- TODO: use findPluginModule?
lookupModule' mod pkg = do
  r <- PluginAPI.findImportedModule mod PluginAPI.NoPkgQual --  (PluginAPI.OtherPkg pkg)
  case r of
    PluginAPI.Found _ md -> pure md
    _ -> do r' <- PluginAPI.findImportedModule mod PluginAPI.NoPkgQual
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


evByFiatExpr s t1 t2 = evTermToExpr $ PluginAPI.mkPluginUnivEvTerm s Nominal t1 t2 

evTermToExpr :: EvTerm -> EvExpr
evTermToExpr (EvExpr e) = e
evTermToExpr _ = error "evTermToExpr"



tcPluginTrace s d = PluginAPI.Internal.unsafeLiftTcM $ GHC.traceTc s d
