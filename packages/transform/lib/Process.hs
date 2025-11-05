module Process where

import Options.Applicative (Parser, execParser, header, info, long, strOption)
import Parse (collectFiles)
import Transform (transform)
import Write (writeTarget)

data Options =
  Options {
    originalDir :: Maybe FilePath,
    jsonDir :: FilePath,
    outputDir :: FilePath,
    prefix :: Maybe Text
  }
  deriving stock (Eq, Show)

process :: Options -> IO ()
process Options {..} = do
  targets <- collectFiles (toText <$> originalDir) jsonDir
  when False do
    dbgs targets
  let transformed = transform targets
  when False do
    dbgs transformed
  traverse_ (writeTarget outputDir (Just (toText outputDir))) transformed

parser :: Parser Options
parser = do
  originalDir <- optional (strOption (long "original"))
  jsonDir <- strOption (long "json")
  outputDir <- strOption (long "out")
  prefix <- optional (strOption (long "prefix"))
  pure Options {..}

processCli :: IO ()
processCli = do
  options <- execParser (info parser (header "Strip a buck project down to its dependency graph"))
  process options

test_tree1 :: IO ()
test_tree1 =
  process Options {
    originalDir = Just "tests/tree1/original",
    jsonDir = "tests/tree1/json",
    outputDir = "tests/tree1/out",
    prefix = Nothing
  }
