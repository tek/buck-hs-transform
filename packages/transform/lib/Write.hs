module Write where

import Data (Dep (..), DepName (..), Module (..), ModuleName (..), Target (..), TargetName (..))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Exon (exon)
import Paths (buckName, osPathString)
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.OsPath (takeFileName)

formatDep :: Maybe Text -> Dep -> Text
formatDep prefix = \case
  Local (TargetName name) -> [exon|//#{foldMap prefixDir prefix}#{name}:#{name}|]
  External (DepName name) -> [exon|//haskell:#{name}|]
  where
    prefixDir path = [exon|#{path}/|]

renderTarget ::
  Maybe Text ->
  Target Module ->
  Text
renderTarget prefix Target {outputName = TargetName outputName, ..} =
  [exon|
# Originally ##{inputName}
haskell_lib(
    name = #{quoted outputName}
    srcs = #{attrList (renderModule <$> modules)},
    deps = #{attrList (quoted . formatDep prefix <$> deps)},
    visibility = ["PUBLIC"],
)|]
  where
    renderModule Module {outputPath} = quoted (toText (osPathString (takeFileName outputPath)))

    quoted name = [exon|"#{name}",|]

    attrList items = "[" <> Text.intercalate "\n        " ("" : items) <> "\n    ]"

buckContents ::
  Maybe Text ->
  Target Module ->
  Text
buckContents prefix target =
  [exon|load("//haskell:custom.bzl", "haskell_lib")
#{renderTarget prefix target}|]

writeModule :: FilePath -> Module -> IO ()
writeModule dir Module {inputModuleName, outputModuleName = ModuleName name, imports} =
  Text.writeFile (dir FilePath.</> toString name ++ ".hs") contents
  where
    contents =
      [exon|-- Originally ##{inputModuleName}
module #{name} where

#{Text.unlines (renderImport <$> imports)}
|]

    renderImport (ModuleName iname) = [exon|import #{iname}|]

writeTarget ::
  FilePath ->
  Maybe Text ->
  Target Module ->
  IO ()
writeTarget outputDir prefix target@Target {outputName = TargetName name, modules} = do
  Directory.removePathForcibly dir
  Directory.createDirectoryIfMissing True dir
  Text.writeFile (dir FilePath.</> osPathString buckName) (buckContents prefix target)
  traverse_ (writeModule dir) modules
  where
    dir = outputDir FilePath.</> toString name
