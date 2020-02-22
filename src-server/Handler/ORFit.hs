{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.ORFit where

import qualified Aws
import qualified Aws.Core as Core
import qualified Aws.S3 as S3
import qualified Data.Aeson as A
import qualified Data.UUID as UUID
import Aws.Lambda
import Data.Conduit ((.|), runConduit, mapOutput)
import Data.Conduit.Binary (sinkLbs)
import Data.Conduit.Text(decode)
import Data.Map
import Data.Text as T
import GHC.Generics
import Network.HTTP.Conduit (responseBody, RequestBody(..))
import System.Random (randomIO)

import File.ANGReader as R

import Type.APIGateway
import Util.AWS

handler :: Event String -> Context -> IO (Either String (Response Text))
handler Event{..} _ = do
  return $ Left "WIP"

orFitHandler :: Text -> IO (Either String ())
orFitHandler angHash = do
  let
    s3cfg :: S3.S3Configuration Aws.NormalQuery
    s3cfg = S3.s3v4 Core.HTTPS "s3.us-east-2.amazonaws.com" False S3.AlwaysUnsigned

  let ang_bucket = S3.getObject "gamma-builder-inputs" angHash
  let body = RequestBodyLBS "ddd"
  let vox_bucket = S3.putObject "gamma-builder-inputs" (angHash <> "-out") body
    
  ang <- runAWSWith s3cfg ang_bucket $ \(S3.GetObjectResponse { S3.gorResponse = rsp }) -> do
    runConduit $ responseBody rsp .| sinkLbs
      
  rsp <- runAWS s3cfg vox_bucket
      
  return $ case R.loadANG ang of
    Right _ -> Right ()
    Left err -> Left err
  