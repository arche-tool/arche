{-# LANGUAGE OverloadedStrings #-}

module Handler.SubmitANG where

import qualified Aws
import qualified Aws.Core as Core
import qualified Aws.S3 as S3
import           Data.Conduit ((.|), runConduit)
import           Data.Conduit.Binary (sinkFile)
import           Network.HTTP.Conduit (responseBody)

import Util.AWS

uploadANG :: IO ()
uploadANG = do
  let
    s3cfg :: S3.S3Configuration Aws.NormalQuery
    s3cfg = S3.s3v4 Core.HTTPS "s3.us-east-2.amazonaws.com" False S3.AlwaysUnsigned

  let bucket = S3.getObject "gamma-builder-inputs" "test.vtr"
  runAWSWith s3cfg bucket $ \(S3.GetObjectResponse { S3.gorResponse = rsp }) -> do
    runConduit $ responseBody rsp .| sinkFile "text.vtr"