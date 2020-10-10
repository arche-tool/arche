{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Util.FireStore
    ( GCP
    , runGCPWith
    , toDoc
    , fromDoc
    , module Util.FireStore.Value
    , module Util.FireStore.Document
    ) where

import Control.Exception            (try)
import Control.Lens                 ((.~), (<&>))
import Control.Monad.Catch          (catch, throwM)
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Network.HTTP.Types.Status    (statusCode)
import Servant.Server               (err400, err404, err500, err502)
import Servant                      (Handler, throwError, ServerError(..))
import System.IO                    (stdout)

import qualified Network.Google           as Google
import qualified Network.Google.FireStore as FireStore

import Util.FireStore.Document
import Util.FireStore.Value
import Util.Logger

-- ============== Google Cloud ================
type GCP = '["https://www.googleapis.com/auth/cloud-platform"]


runGCPWith :: Google.Google GCP a -> Servant.Handler a
runGCPWith gcp = do
  result <- liftIO (try action)
  either serverErrorHandler return result
  where
    action = do
      lgr  <- Google.newLogger Google.Info stdout

      env  <- Google.newEnv <&>
            (Google.envLogger .~ lgr)
          . (Google.envScopes .~ FireStore.cloudPlatformScope)

      runResourceT $ Google.runGoogle env (catch gcp googleErrorHandler)

serverErrorHandler :: ServerError -> Servant.Handler a 
serverErrorHandler err = do
  let
    httpCode = errHTTPCode err 
    logType  = if httpCode >= 400 && httpCode < 500 then INFO else ERROR 
  liftIO . printLog logType $ logMsg ("Exiting with" :: String) httpCode ("(" <> errReasonPhrase err <> ")")
  throwError err

googleErrorHandler :: Google.Error -> Google.Google GCP a
googleErrorHandler err = case err of
  Google.TransportError httpException  -> logGGError (logMsg $ show httpException)  >> throwM err500
  Google.SerializeError serializeError -> logGGError (logMsg $ show serializeError) >> throwM err500 
  Google.ServiceError   serviceError
    | httpStatus == 404 -> logGGInfo  (logMsg $ show serviceError) >> throwM err404
    | httpStatus == 400 -> logGGInfo  (logMsg $ show serviceError) >> throwM err400
    | otherwise         -> logGGError (logMsg $ show serviceError) >> throwM err502
    where httpStatus = statusCode (Google._serviceStatus serviceError)

-- ============== Document store ================
toDoc :: ToDocValue a => a -> FireStore.Document
toDoc = buildDocFromValue . toValue

fromDoc :: FromDocValue a => FireStore.Document -> Either String a
fromDoc doc = buildValueFromDoc doc >>= fromValue 
