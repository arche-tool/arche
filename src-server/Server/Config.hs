{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Server.Config where

import Data.Aeson    (FromJSON(..))
import Data.Text     (Text)
import GHC.Generics

import qualified Data.Aeson as A

data ArcheServerConfig
    = ArcheServerConfig
    { oauthContext :: Text
    } deriving Generic

instance FromJSON ArcheServerConfig

readArcherServerConfig :: FilePath -> IO (Either String ArcheServerConfig)
readArcherServerConfig = A.eitherDecodeFileStrict'