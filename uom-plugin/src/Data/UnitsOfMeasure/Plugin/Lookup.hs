{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Data.UnitsOfMeasure.Plugin.Lookup
  ( lookupUnitDefs
  ) where

import GhcApi (occNameFS, occName, tyConDataCons, dataConName)

import GHC.TcPlugin.API

import Data.UnitsOfMeasure.Plugin.Convert

lookupUnitDefs :: TcPluginM Init UnitDefs
lookupUnitDefs = do
    md <- lookModule
    unitTagKindCon            <- lookTyCon md "UnitKind"
    unitTypeSyn               <- lookTyCon md "Unit"
    unitBaseTyCon             <- lookTyCon md "Base"
    unitOneTyCon              <- lookTyCon md "One"
    mulTyCon                  <- lookTyCon md "*:"
    divTyCon                  <- lookTyCon md "/:"
    expTyCon                  <- lookTyCon md "^:"
    unpackTyCon               <- lookTyCon md "Unpack"
    packTyCon                 <- lookTyCon md "Pack"
    unitSyntaxTyCon           <- lookTyCon md "UnitSyntax"
    equivTyCon                <- lookTyCon md "~~"
    unitSyntaxPromotedDataCon <- findDataCon unitSyntaxTyCon ":/"
    pure UnitDefs{..}

lookModule :: TcPluginM Init Module
lookModule = do
    r <- findImportedModule (mkModuleName mod_name) NoPkgQual
    case r of
        Found _ md -> pure md
        _          -> error $ "uom-plugin: lookModule: not Found: " ++ mod_name
  where
    mod_name = "Data.UnitsOfMeasure.Internal"

lookTyCon :: Module -> String -> TcPluginM Init TyCon
lookTyCon md s = tcLookupTyCon =<< lookupOrig md (mkTcOcc s)

-- | Search the TyCon for a DataCon of the given name, and promote it to a TyCon.
findDataCon :: TyCon -> String -> TcPluginM Init TyCon
findDataCon tycon s = case [ dc | dc <- tyConDataCons tycon, occNameFS (occName (dataConName dc)) == fsLit s ] of
    [d] -> pure (promoteDataCon d)
    _   -> error $ "uom-plugin: findDataCon: missing " ++ s
