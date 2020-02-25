{-# LANGUAGE OverloadedStrings #-}

module Util.AWS where

import qualified Aws
import qualified Aws.Core as Core
import qualified Data.Map
import           Control.Monad.Trans.Resource
import           Network.HTTP.Conduit (newManager, tlsManagerSettings)

import Type.APIGateway (Response(..))

runAWS :: (Core.Transaction a o) => Core.ServiceConfiguration a Core.NormalQuery -> a -> IO o
runAWS serviceCfg serviceObj = do
  cfg <- Aws.baseConfiguration
  mgr <- newManager tlsManagerSettings
  runResourceT $ do
    Aws.pureAws cfg serviceCfg mgr serviceObj

runAWSWith :: (Core.Transaction a o) => Core.ServiceConfiguration a Core.NormalQuery -> a -> (o -> ResourceT IO b) -> IO b
runAWSWith serviceCfg serviceObj func = do
  cfg <- Aws.baseConfiguration
  mgr <- newManager tlsManagerSettings
  runResourceT $ do
    o <- Aws.pureAws cfg serviceCfg mgr serviceObj
    func o

okJson :: a -> Response a
okJson rsp = Response
    { statusCode = 200
    , body = rsp
    , isBase64Encoded = False
    , headers = Data.Map.singleton "Content-Type" "application/json"
    }