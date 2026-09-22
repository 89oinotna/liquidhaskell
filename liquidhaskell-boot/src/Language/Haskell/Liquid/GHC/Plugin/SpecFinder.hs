{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes   #-}

{-
Note [Module Visibility and Lookup in GHC]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC distinguishes between two module visibility namespaces:

- **Regular packages** (`-package`): modules are found by `findImportedModule`,
  which searches `findExposedPackageModule`.
- **Plugin packages** (`-plugin-package`): modules are found by
  `findPluginModule`, which searches `findExposedPluginPackageModule`.

Whenever a module is looked up, we start with `findImportedModule` to check
regular packages, and if that fails, we fall back to `findPluginModule` to check
plugin packages. This allows us to support both visibility namespaces without
requiring users to mind how they specify dependencies.

-}

module Language.Haskell.Liquid.GHC.Plugin.SpecFinder
    ( findRelevantSpecs
    , configToRedundantDependencies
    ) where

import qualified Language.Haskell.Liquid.GHC.Plugin.Serialisation as Serialisation
import           Language.Haskell.Liquid.GHC.Plugin.Types
import           Language.Haskell.Liquid.Types.Specs
import           Language.Haskell.Liquid.UX.Config

import           Liquid.GHC.API         as GHC

import           Data.Bifunctor
import qualified Data.Char
import           Data.Maybe
import           Control.Monad (foldM, unless)
import qualified Data.HashMap.Strict as HM
import           Data.List (sortOn)

-- Force the target when inserting it, as the old strict map did. Otherwise a
-- selector thunk can retain the entire decoded library and its reference list.
data LoadedSpec = LoadedSpec !Bool !SpecReference !LiftedSpec

-- | Load any relevant spec for the input list of 'Module's, by querying both the 'ExternalPackageState'
-- and the 'HomePackageTable'.
--
-- Specs come from the interface files of the given modules or their matching
-- _LHAssumptions modules. A module @M@ only matches with a module named
-- @M_LHAssumptions@.
--
-- Assumptions are taken from _LHAssumptions modules only if the interface
-- file of the matching module contains no spec.
findRelevantSpecs :: [String] -- ^ Package to exclude for loading LHAssumptions
                  -> HscEnv
                  -> [Module]
                  -- ^ Any relevant module fetched during dependency-discovery.
                  -> TcM (TargetDependencies, [SpecReference])
findRelevantSpecs lhAssmPkgExcludes hscEnv mods = do
    entries <- foldM loadAndMerge HM.empty mods
    pure ( TargetDependencies $ HM.map (\(LoadedSpec _ _ spec) -> spec) entries
         , sortOn specModule [ref | LoadedSpec _ ref _ <- HM.elems entries]
         )
  where
    -- Preserve discovery order (loading assumptions mutates GHC's EPS), and
    -- reproduce the old reverse/fold precedence without retaining all results:
    -- the first direct target wins; otherwise the last dependency entry wins.
    loadAndMerge entries currentModule = do
      found <- loadRelevantSpec currentModule
      case found of
        Nothing -> pure entries
        Just (ref, lib) -> do
          entries' <- foldM mergeDependency entries (libDeps lib)
          let key = specModule ref
              merged = case HM.lookup key entries' of
                Just (LoadedSpec True _ _) -> entries'
                Just (LoadedSpec False oldRef spec) | oldRef == ref ->
                  HM.insert key (LoadedSpec True ref spec) entries'
                _ -> HM.insert key (LoadedSpec True ref $ libTarget lib) entries'
          -- foldM does not force its accumulator. Complete this merge before
          -- loading the next interface, including imports with no dependencies.
          merged `seq` pure merged

    mergeDependency entries ref = case HM.lookup (specModule ref) entries of
      Just (LoadedSpec True actual _) -> do
        checkReference ref actual
        pure entries
      Just (LoadedSpec _ oldRef _) | oldRef == ref -> pure entries
      _ -> do
        let mdl = unStableModule $ specModule ref
        -- References include package/unit identity and the exact saved spec
        -- fingerprint. Never resolve them by an unqualified module name.
        _ <- initIfaceTcRn $ loadInterface "liquidhaskell dependency" mdl ImportBySystem
        found <- liftIO $ Serialisation.deserialiseLiquidLib hscEnv mdl
        case found of
          Just (actual, lib) -> do
            checkReference ref actual
            let merged = HM.insert (specModule ref) (LoadedSpec False ref $ libTarget lib) entries
            merged `seq` pure merged
          Nothing -> failWithTc $ mkTcRnUnknownMessage $ mkPlainError [] $
            text "LiquidHaskell: missing dependency specification; rebuild dependencies:" <+> ppr mdl

    checkReference ref actual = unless (actual == ref) $
      failWithTc $ mkTcRnUnknownMessage $ mkPlainError [] $
        text "LiquidHaskell: stale dependency specification; rebuild the importing module:" <+>
        ppr (unStableModule $ specModule ref)

    loadRelevantSpec :: Module -> TcM (Maybe (SpecReference, LiquidLib))
    loadRelevantSpec currentModule = do
      res <- liftIO $ Serialisation.deserialiseLiquidLib hscEnv currentModule
      case res of
        Nothing -> loadModuleLHAssumptionsIfAny currentModule
        Just _ -> pure res

    loadModuleLHAssumptionsIfAny m | isImportExcluded m = return Nothing
                                   | otherwise = do
      let assumptionsModName = assumptionsModuleName m
      -- loadInterface might mutate the EPS if the module is
      -- not already loaded.
      --
      -- Try findImportedModule first (for -package), then fall back to
      -- findPluginModule (for -plugin-package).
      -- See Note [Module Visibility and Lookup in GHC] for details.
      res <- liftIO $ do
        r <- findImportedModule hscEnv assumptionsModName NoPkgQual
        case r of
          Found{} -> pure r
          _       -> findPluginModule hscEnv assumptionsModName
      case res of
        Found _ assumptionsMod -> do
          _ <- initIfaceTcRn $ loadInterface "liquidhaskell assumptions" assumptionsMod ImportBySystem
          liftIO $ Serialisation.deserialiseLiquidLib hscEnv assumptionsMod
        FoundMultiple{} -> failWithTc $ mkTcRnUnknownMessage $ mkPlainError [] $
                             missingInterfaceErrorDiagnostic (initIfaceMessageOpts $ hsc_dflags hscEnv) $
                             cannotFindModule hscEnv assumptionsModName res
        _ -> return Nothing

    isImportExcluded m =
      let s = takeWhile Data.Char.isAlphaNum $ unitString (moduleUnit m)
       in elem s lhAssmPkgExcludes

    assumptionsModuleName m =
      mkModuleNameFS $ moduleNameFS (moduleName m) <> "_LHAssumptions"

-- | Returns a list of 'StableModule's which can be filtered out of the dependency list, because they are
-- selectively \"toggled\" on and off by the LiquidHaskell's configuration, which granularity can be
-- /per module/.
configToRedundantDependencies :: HscEnv -> Config -> IO [StableModule]
configToRedundantDependencies env cfg = do
  catMaybes <$> mapM (lookupModule' . first ($ cfg)) configSensitiveDependencies
  where
    lookupModule' :: (Bool, ModuleName) -> IO (Maybe StableModule)
    lookupModule' (fetchModule, modName)
      | fetchModule = lookupLiquidBaseModule modName
      | otherwise   = pure Nothing

    lookupLiquidBaseModule :: ModuleName -> IO (Maybe StableModule)
    lookupLiquidBaseModule mn = do
      res <- findImportedModule env mn (renamePkgQual (hsc_unit_env env) mn (Just "liquidhaskell"))
      case res of
        Found _ mdl -> pure $ Just (toStableModule mdl)
        _ -> do
          -- Fall back to plugin package visibility
          -- See Note [Module Visibility and Lookup in GHC] for details.
          res2 <- findPluginModule env mn
          case res2 of
            Found _ mdl -> pure $ Just (toStableModule mdl)
            _           -> pure Nothing

-- | Static associative map of the 'ModuleName' that needs to be filtered from the final 'TargetDependencies'
-- due to some particular configuration options.
--
-- Modify this map to add any extra special case. Remember that the semantic is not which module will be
-- /added/, but rather which one will be /removed/ from the final list of dependencies.
--
configSensitiveDependencies :: [(Config -> Bool, ModuleName)]
configSensitiveDependencies = [
    (not . totalityCheck, mkModuleName "Liquid.Prelude.Totality_LHAssumptions")
  , (linear, mkModuleName "Liquid.Prelude.Real_LHAssumptions")
  ]
