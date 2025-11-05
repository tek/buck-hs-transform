module Transform where

import Control.Monad.Trans.State.Strict (StateT, evalState, state)
import Data (
  Dep (..),
  DepName (..),
  ExternalDep (..),
  HsFile (..),
  Module (..),
  ModuleName (..),
  OriginalModule (..),
  Target (..),
  TargetName (..),
  )
import Data.List.Extra (nubOrd)
import qualified Data.Map.Strict as Map
import System.OsPath (OsPath, encodeUtf, (<.>), (</>))

osPath :: String -> OsPath
osPath = fromMaybe (error "OsPath") . encodeUtf

data LookupTable =
  LookupTable {
    moduleNames :: Map ModuleName (ModuleName, TargetName)
  }
  deriving stock (Eq, Show)

renameImport ::
  LookupTable ->
  ModuleName ->
  StateT Int Identity (Either (ModuleName, TargetName) ExternalDep)
renameImport table imod =
  maybe externalDep localDep (table.moduleNames Map.!? imod)
  where
    externalDep =
      state \ i -> (Right (ExternalDep (i + 1)), fromMaybe 0 (mod (i + 1) 100))

    localDep (newName, libName) = do
      pure (Left (newName, libName))

assembleDeps ::
  TargetName ->
  [(ModuleName, TargetName)] ->
  [ExternalDep] ->
  ([ModuleName], [Dep])
assembleDeps target local external =
  (externalImports ++ localImports, External (DepName "base") : externalDeps ++ nubOrd localDeps)
  where
    (externalImports, externalDeps) =
      unzip [("Dep" <> show i, External ("dep" <> show i)) | ExternalDep i <- nubOrd external]

    (localImports, localDeps) = second (mapMaybe notHomeUnit) (unzip local)

    notHomeUnit dep =
      if dep == target
      then Nothing
      else Just (Local dep)

renameModuleImports ::
  LookupTable ->
  TargetName ->
  Module ->
  StateT Int Identity (Module, [Dep])
renameModuleImports table target Module {..} = do
  (local, external) <- partitionEithers <$> traverse (renameImport table) imports
  let (renamedImports, deps) = assembleDeps target local external
  pure (Module {
    inputPath,
    outputPath,
    inputModuleName,
    outputModuleName,
    imports = renamedImports
  }, deps)

renameModule ::
  Int ->
  HsFile ->
  Int ->
  Module
renameModule targetNum HsFile {path, omodule = OriginalModule {..}} moduleNum =
  Module {
    inputPath = path,
    outputPath = osPath ("lib" ++ show targetNum) </> osPath (toString newName) <.> osPath "hs",
    inputModuleName = module_name,
    outputModuleName = ModuleName newName,
    imports
  }
  where
    newName = "Lib" <> show targetNum <> "Mod" <> show moduleNum

lookupTable :: [Target Module] -> LookupTable
lookupTable targets =
  LookupTable {moduleNames}
  where
    moduleNames = Map.unions (step <$> targets)

    step Target {modules, outputName} =
      Map.fromList [
        (inputModuleName, (outputModuleName, outputName))
          | Module {inputModuleName, outputModuleName}
          <- modules
      ]

renameDeps ::
  LookupTable ->
  Target Module ->
  StateT Int Identity (Target Module)
renameDeps table Target {..} = do
  (renamedModules, renamedDeps) <- fmap concat . unzip <$> traverse (renameModuleImports table outputName) modules
  pure Target {
    inputName,
    outputName,
    srcs,
    modules = renamedModules,
    deps = sort (nubOrd renamedDeps)
  }

renameTarget ::
  Int ->
  Target HsFile ->
  Target Module
renameTarget targetNum Target {outputName = _, ..} =
  Target {
    inputName,
    outputName = TargetName ("lib" <> show targetNum),
    srcs,
    modules = renamedModules,
    deps
  }
  where
    renamedModules = zipWith (renameModule targetNum) modules [1..]

transform :: [Target HsFile] -> [Target Module]
transform targets =
  evalState (traverse (renameDeps (lookupTable renamed)) renamed) 1
  where
    renamed = zipWith renameTarget [1..] targets
