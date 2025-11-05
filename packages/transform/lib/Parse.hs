module Parse where

import Control.Exception (throwIO)
import Data (HsFile (..), SrcEntry (SrcEntry, exclude, value), Target (..), TargetName, TargetSpec (..))
import Data.Aeson (FromJSON (..), Value (..), eitherDecodeFileStrict', withObject, (.:))
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Paths (osPathString, stripPackage)
import System.Directory (doesFileExist)
import qualified System.FilePath as FilePath
import System.FilePattern ((?==))
import System.IO (stderr)
import System.IO.Error (userError)
import System.OsPath (OsPath, decodeFS)
import Transform (osPath)

note :: String -> Maybe a -> IO a
note msg = \case
  Just a -> pure a
  Nothing -> throwIO (userError msg)

fromEither :: (b -> String) -> Either b a -> IO a
fromEither msg = \case
  Right a -> pure a
  Left b -> throwIO (userError (msg b))

parseError ::
  String ->
  String ->
  String
parseError path msg =
  "JSON parse error in " ++ path ++ ": " ++ msg

parseJson ::
  FromJSON a =>
  String ->
  FilePath ->
  IO a
parseJson errPath path =
  fromEither (parseError errPath) =<< eitherDecodeFileStrict' path

hsNode ::
  OsPath ->
  OsPath ->
  OsPath ->
  IO ([Target HsFile], [a], Map String HsFile)
hsNode absPath relPath name = do
  fp <- decodeFS absPath
  omodule <- parseJson (show relPath) fp
  pure ([], [], [(osPathString name, HsFile {path = relPath, omodule})])

buckNode ::
  OsPath ->
  OsPath ->
  OsPath ->
  IO ([Target HsFile], [TargetSpec], Map String HsFile)
buckNode absPath relPath _ = do
  fp <- decodeFS absPath
  specs <- parseJson (show relPath) fp
  pure ([], specs, [])

matchSourceSpec ::
  ([HsFile], Map String HsFile) ->
  SrcEntry ->
  ([HsFile], Map String HsFile)
matchSourceSpec (matching, remaining) SrcEntry {value, exclude} =
  (Map.elems predMatch ++ matching, newRemaining)
  where
    (predMatch, newRemaining) = Map.partitionWithKey globMatch remaining

    globMatch rel _ = value ?== rel && maybe True (not . any (== rel)) exclude

data LibTargetAttrs =
  LibTargetAttrs {
    name :: TargetName,
    srcs :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data FileTargetAttrs =
  FileTargetAttrs {
    name :: TargetName,
    src :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data TargetAttrs =
  Lib LibTargetAttrs
  |
  File FileTargetAttrs
  deriving stock (Eq, Show)

instance FromJSON TargetAttrs where
  parseJSON =
    withObject "TargetAttrs" \ o -> do
      o .: "buck.type" >>= \case
        ("haskell_library" :: Text) -> Lib <$> parseJSON (Object o)
        "export_file" -> File <$> parseJSON (Object o)
        t -> fail ("Unexpected target type " ++ toString t)

targetAttrsEither ::
  TargetAttrs ->
  Either LibTargetAttrs FileTargetAttrs
targetAttrsEither = \case
  Lib a -> Left a
  File a -> Right a

sanitize :: TargetAttrs -> TargetAttrs
sanitize = \case
  Lib LibTargetAttrs {..} -> Lib LibTargetAttrs {srcs = stripPackage <$> srcs, ..}
  File FileTargetAttrs {..} -> File FileTargetAttrs {src = stripPackage src, ..}

parseProjectConfig :: FilePath -> IO (Map Text TargetAttrs)
parseProjectConfig jsonDir = do
  attrs <- fromEither (parseError name) =<< eitherDecodeFileStrict' path
  pure (sanitize <$> Map.mapKeys stripPackage attrs)
  where
    path = jsonDir FilePath.</> name

    name = "buck.json"

resolveModule ::
  Text ->
  FilePath ->
  Text ->
  IO (Maybe HsFile)
resolveModule prefix jsonDir path = do
  localPath <- note ("no dir prefix on " ++ toString path) (Text.stripPrefix prefix path)
  let jsonPath = jsonDir FilePath.</> toString localPath
  doesFileExist jsonPath >>= \case
    False -> do
      Text.hPutStrLn stderr ("Nonexistent module: " <> localPath)
      pure Nothing
    True -> do
      omodule <- parseJson (show localPath) jsonPath
      pure $ Just HsFile {
        path = osPath (toString localPath),
        omodule
      }

createTarget ::
  Text ->
  FilePath ->
  LibTargetAttrs ->
  IO (Target HsFile)
createTarget prefix jsonDir LibTargetAttrs {name, srcs} = do
  modules <- catMaybes <$> traverse (resolveModule prefix jsonDir) srcs
  pure Target {
    inputName = name,
    outputName = name,
    srcs = [],
    modules,
    deps = []
  }

resolveSrcs ::
  Map Text LibTargetAttrs ->
  Map Text FileTargetAttrs ->
  [LibTargetAttrs]
resolveSrcs libs files =
  Map.elems libs <&> \ LibTargetAttrs {..} ->
    LibTargetAttrs {srcs = resolveSrc <$> srcs, ..}
  where
    resolveSrc label =
      maybe label (.src) (files !? label)

groupTargets ::
  Maybe Text ->
  FilePath ->
  Map Text TargetAttrs ->
  IO [Target HsFile]
groupTargets originalDir jsonDir targets =
  traverse (createTarget (foldMap prefix originalDir) jsonDir) resolved
  where
    resolved = resolveSrcs libs files

    (libs, files) = Map.mapEither targetAttrsEither targets

    prefix path =
      Text.dropWhileEnd (== '/') path <> "/"

collectFiles ::
  Maybe Text ->
  FilePath ->
  IO [Target HsFile]
collectFiles originalDir jsonDir = do
  attrs <- parseProjectConfig jsonDir
  groupTargets originalDir jsonDir attrs
