{-# LANGUAGE
    BangPatterns
  , DeriveAnyClass
  , DeriveGeneric
  , DuplicateRecordFields
  , OverloadedStrings
  , RecordWildCards
  #-}

module Handler.ORFit where

import qualified Aws
import qualified Aws.Core as Core
import qualified Aws.S3 as S3
import qualified Data.Aeson as A
import qualified Data.UUID as UUID
import qualified Data.ByteString.Lazy         as BSL
import Aws.Lambda
import Data.Conduit ((.|), runConduit, mapOutput)
import Data.Conduit.Binary (sinkLbs)
import Data.Conduit.Text(decode)
import Data.Map
import Data.Text as T
import GHC.Generics
import Network.HTTP.Conduit (responseBody, RequestBody(..))

import qualified Arche.Strategy.ORFitAll as OR
import Arche.Strategy.ORFitAll (OREvaluation)
import Data.VTK (renderUniVTK, writeUniVTKfile)
import Texture.Orientation (Deg(..))

import Type.APIGateway
import Util.AWS
import Util.OrphanInstances ()

handler :: Event Text -> Context -> IO (Either String (Response OREvaluation))
handler Event{..} _ = do
  rsp <- orFitHandler body
  return $ fmap okJson rsp

orFitHandler :: Text -> IO (Either String OREvaluation)
orFitHandler angHash = do
  let
    s3cfg :: S3.S3Configuration Aws.NormalQuery
    s3cfg = S3.s3v4 Core.HTTPS "s3.us-east-2.amazonaws.com" False S3.AlwaysUnsigned

  let ang_bucket = S3.getObject "gamma-builder-inputs" angHash
    
  ang <- runAWSWith s3cfg ang_bucket $ \(S3.GetObjectResponse { S3.gorResponse = rsp }) -> do
    runConduit $ responseBody rsp .| sinkLbs
      
  let cfg = OR.Cfg { OR.misoAngle    = Deg 5
                   , OR.optByAvg     = False
                   , OR.predefinedOR = Nothing
                   }

  -- Force strictness on OR calculation otherwise the data
  --upload bellow can timeout while awaiting for calculation.
  (!orEval, vtk) <- OR.processEBSD cfg ang
  
  let body = RequestBodyLBS (renderUniVTK True vtk)
  let vox_bucket = S3.putObject "gamma-builder-inputs" (angHash <> "-out") body
  rsp <- runAWS s3cfg vox_bucket
  
  return $ Right orEval
  