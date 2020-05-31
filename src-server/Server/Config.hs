{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ExistentialQuantification #-}

module Server.Config
  ( ArcheServerConfig
    ( oauth_client_id
    , run_mode
    , port
    , signerEncodedCredentials
    )
  , RunMode(..)
  , loadConfig
  , readArcherServerConfig  
  ) where

import Data.Aeson           (FromJSON(..))
import Data.Text            (Text, pack)
import GHC.Generics
import Options.Applicative

import qualified Data.Aeson as A

readArcherServerConfig :: FilePath -> IO (Either String ArcheServerConfig)
readArcherServerConfig = A.eitherDecodeFileStrict'

-- ===================================== Data & class ====================================

data RunMode
    = ComputeAPI
    | NonComputeAPI
    | FullAPI
    deriving (Show, Generic)

data ArcheServerConfig
    = ArcheServerConfig
    { oauth_client_id          :: Text
    , run_mode                 :: RunMode
    , port                     :: Int
    , signerEncodedCredentials :: Maybe Text
    } deriving (Show, Generic)

instance FromJSON RunMode
instance FromJSON ArcheServerConfig

-- ========================================= Main ========================================

parseRunMode :: Parser RunMode
parseRunMode = subparser
 ( command "compute-api"
   (info (pure ComputeAPI)
   (progDesc "Only expose compute API that execute under heavy CPU load."))
 <> command "non-compute-api"
   (info (pure NonComputeAPI)
   (progDesc "Only expose non-compute API that is lightweight on resources."))
 <> command "full-api"
   (info (pure FullAPI)
   (progDesc "Expose the full API."))
 )

parseConfig :: Parser ArcheServerConfig
parseConfig = ArcheServerConfig
  <$> parseOAuthClientID
  <*> parseRunMode
  <*> parsePort
  <*> parseSignerEncodedCredentials

parseConfigFilepath :: Parser FilePath
parseConfigFilepath = strOption
   (  long "config-file"
   <> short 'f'
   <> help "Path for the config file in JSON format." )

parseEitherConfigOrPath :: Parser (Either FilePath ArcheServerConfig)
parseEitherConfigOrPath = (Right <$> parseConfig) <|> (Left <$> parseConfigFilepath) 

loadConfig :: IO ArcheServerConfig
loadConfig = do
  cfgOrFile <- execParser opts :: IO (Either FilePath ArcheServerConfig)
  case cfgOrFile of
    Right cfg -> return cfg
    Left path -> do
      readArcherServerConfig path >>= either error return 
  where
    opts = info (helper <*> parseEitherConfigOrPath)
           ( fullDesc
           <> progDesc "Arche Server exposes an API to perform parent phase reconstruction on metals."
           <> header "Arche Builder")

-- ======================================= Common tools ==================================

parseOAuthClientID :: Parser Text
parseOAuthClientID = pack <$> strOption
   (  long "oauth-client-id"
   <> short 'o'
   <> metavar "OAUTH_CLIENT_ID"
   <> help "OAuth client id used to verify signed users.")

parsePort :: Parser Int
parsePort = option auto
   (  long "port"
   <> short 'p'
   <> metavar "port"
   <> value 8080
   <> help "Service port.")

parseSignerEncodedCredentials :: Parser (Maybe Text)
parseSignerEncodedCredentials = fmap pack <$> (optional . strOption)
   (  long "signer-credentials"
   <> short 's'
   <> metavar "signer-credentials"
   <> help "GCS service account for signing URLs in JSON format en encode in base64.")