{-# LANGUAGE OverloadedStrings #-}

import qualified Aws
import qualified Aws.Core as Core
import qualified Aws.S3 as S3
import           Control.Monad.Trans.Resource
import           Data.Conduit ((.|), runConduit)
import           Data.Conduit.Binary (sinkFile)
import           Network.HTTP.Conduit (newManager, tlsManagerSettings, responseBody)

main :: IO ()
main = do
  cfg <- Aws.baseConfiguration
  let
    s3cfg :: S3.S3Configuration Aws.NormalQuery
    s3cfg = S3.s3v4 Core.HTTPS "s3.us-east-2.amazonaws.com" False S3.AlwaysUnsigned

  mgr <- newManager tlsManagerSettings
  runResourceT $ do
    S3.GetObjectResponse { S3.gorResponse = rsp } <- Aws.pureAws cfg s3cfg mgr $ S3.getObject "gamma-builder-inputs" "test.vtr"
    runConduit $ responseBody rsp .| sinkFile "text.vtr"