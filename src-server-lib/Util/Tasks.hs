
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeOperators       #-}

module Util.Tasks
  ( createSelfTaskRequest
  , submitSelfTask
  ) where

import Control.Lens                 ((&), (?~), view)
import Control.Monad.Free
import Data.Binary.Builder          (toLazyByteString)
import Data.Text                    (Text)
import Network.HTTP.Client          (method, requestBody, getUri)
import Network.HTTP.Conduit         (RequestBody(..))
import Servant.Client.Free

import qualified Data.ByteString.Base64    as B64
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.HashMap.Strict       as HM
import qualified Data.Text                 as T
import qualified Network.Google            as Google
import qualified Network.Google.CloudTasks as Tasks

import Type.Storage   (TaskQueue(..))
import Util.FireStore (GCP)
import Util.Logger    (logGGInfo, logMsg)

import qualified Util.Auth   as Auth
import qualified Util.Client as Client

submitSelfTask :: (Show a)=> TaskQueue -> Auth.BearerToken -> Free ClientF a -> Google.Google GCP Text
submitSelfTask queue tk clientI = do
  httpReq <- either fail return $ createSelfTaskRequest tk clientI
  let 
    task = Tasks.task & Tasks.tHTTPRequest ?~ httpReq
    taskReq = Tasks.createTaskRequest & Tasks.ctrTask ?~ task
  resp <- Google.send (Tasks.projectsLocationsQueuesTasksCreate (queueName queue) taskReq)
  name <- maybe (fail "Async task needs a name") return $ view Tasks.tName resp
  logGGInfo $ logMsg ("Sending async task " :: String) name
  return name

createSelfTaskRequest :: (Show a) => Auth.BearerToken -> Free ClientF a -> Either String Tasks.HTTPRequest
createSelfTaskRequest tk clientI = let
  toBS req = case requestBody req of
    RequestBodyLBS lbs     -> return $ BSL.toStrict lbs
    RequestBodyBS   bs     -> return bs
    RequestBodyBuilder _ b -> return . BSL.toStrict . toLazyByteString $ b
    _                      -> fail "Can not build request body."
  toMethod req = case method req of
    "POST"    -> Tasks.HTTPRHTTPMPost'
    "GET"     -> Tasks.HTTPRHTTPMGet'
    "HEAD"    -> Tasks.HTTPRHTTPMHead'
    "PUT"     -> Tasks.HTTPRHTTPMPut'
    "DELETE"  -> Tasks.HTTPRHTTPMDelete'
    "PATCH"   -> Tasks.HTTPRHTTPMPatch'
    "OPTIONS" -> Tasks.HTTPRHTTPMOptions
    _         -> Tasks.HTTPRHTTPMHTTPMethodUnspecified
  headers = HM.fromList [
    ("Authorization", "Bearer " <> Auth.bearerToken tk),
    ("Content-Type", "application/json")
    ]
  in do
    req  <- Client.getRequestOnBaseUrl "https://compute.api.arche.dev" clientI
    body <- toBS req
    return $
      Tasks.hTTPRequest
        & Tasks.httprURL ?~ (T.pack . show . getUri $ req)
        & Tasks.httprBody ?~ (B64.encode body)
        & Tasks.httprHeaders ?~ (Tasks.hTTPRequestHeaders headers)
        & Tasks.httprHTTPMethod ?~ (toMethod req)
