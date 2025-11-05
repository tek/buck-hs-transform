module Paths where

import qualified Data.Text as Text
import System.OsPath (OsPath, decodeUtf, encodeUtf)

osPathString :: OsPath -> String
osPathString = fromMaybe (error "OsPath") . decodeUtf

hsExt :: OsPath
hsExt = fromMaybe (error "hsExt") (encodeUtf ".hs")
{-# noinline hsExt #-}

buckName :: OsPath
buckName = fromMaybe (error "buckName") (encodeUtf "BUCK")
{-# noinline buckName #-}

stripPackage :: Text -> Text
stripPackage path =
  maybe path (Text.dropWhile ('/' ==)) (Text.stripPrefix "root" path)
