{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Web.Config where

import Data.Aeson           ((.:), FromJSON(..))
import Data.Aeson.Types     (parseMaybe)
import Data.HashMap.Strict  (HashMap)
import Data.Text            (Text, unpack, isPrefixOf)
import GHC.Generics
import System.IO            (FilePath)

import qualified Data.Aeson          as A
import qualified Data.HashMap.Strict as HM


data ElmAssets
    = ElmAssets
    { mainJs       :: FilePath
    , mainCss      :: FilePath
    , runtimeJs    :: FilePath
    , vendorJs     :: FilePath
    , staticFiles  :: HashMap Text Text
    } deriving Show

instance FromJSON ElmAssets where
    parseJSON = let
        isStatic k _ = isPrefixOf "static/" k
        in A.withObject "asset-manifest" $ \v -> ElmAssets
        <$> v .: "main.js"
        <*> v .: "main.css"
        <*> v .: "runtime~main.js"
        <*> v .: "vendors~main.js"
        <*> (pure . HM.mapMaybe (parseMaybe parseJSON) . HM.filterWithKey isStatic $ v)

readElmAppConfig :: FilePath -> IO (Either String ElmAssets)
readElmAppConfig = A.eitherDecodeFileStrict'

getStaticAsset :: ElmAssets -> Text -> Maybe FilePath
getStaticAsset asset key = unpack <$> HM.lookup key (staticFiles asset)

data ArcheServerConfig
    = ArcheServerConfig
    { staticFolder      :: FilePath
    , assetManifestFile :: FilePath 
    } deriving Generic

instance FromJSON ArcheServerConfig

readArcherServerConfig :: FilePath -> IO (Either String ArcheServerConfig)
readArcherServerConfig = A.eitherDecodeFileStrict'