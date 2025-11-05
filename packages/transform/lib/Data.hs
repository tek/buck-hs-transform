module Data where

import Data.Aeson (FromJSON)
import System.OsPath (OsPath)

newtype ModuleName =
  ModuleName Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, FromJSON, Semigroup)

newtype TargetName =
  TargetName Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, FromJSON, Semigroup)

newtype DepName =
  DepName Text
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord, FromJSON, Semigroup)

newtype ExternalDep =
  ExternalDep Int
  deriving stock (Eq, Show)
  deriving newtype (Num, Real, Enum, Integral, Ord)

data SrcEntry =
  SrcEntry {
    glob :: Bool,
    value :: String,
    exclude :: Maybe [String]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data TargetSpec =
  TargetSpec {
    name :: TargetName,
    srcs :: [SrcEntry],
    deps :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data OriginalModule =
  OriginalModule {
    module_name :: ModuleName,
    imports :: [ModuleName]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data HsFile =
  HsFile {
    path :: OsPath,
    omodule :: OriginalModule
  }
  deriving stock (Eq, Show)

data Module =
  Module {
    inputPath :: OsPath,
    outputPath :: OsPath,
    inputModuleName :: ModuleName,
    outputModuleName :: ModuleName,
    imports :: [ModuleName]
  }
  deriving stock (Eq, Show)

data Dep =
  External DepName
  |
  Local TargetName
  deriving stock (Eq, Show, Ord)

data Target mod =
  Target {
    inputName :: TargetName,
    outputName :: TargetName,
    srcs :: [SrcEntry],
    modules :: [mod],
    deps :: [Dep]
  }
  deriving stock (Eq, Show)
