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

import Control.Lens                 ((.~), (<&>))
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import System.IO                    (stdout)
import Servant                      (Handler, throwError, ServerError(..))

import qualified Network.Google           as Google
import qualified Network.Google.FireStore as FireStore

import Util.FireStore.Document
import Util.FireStore.Value
import Util.Logger


import Control.Exception (try)

-- ============== Google Cloud ================
type GCP = '["https://www.googleapis.com/auth/cloud-platform"]


runGCPWith :: Google.Google GCP a -> Servant.Handler a
runGCPWith gcp = do
  result <- liftIO (try action)
  either handleServerError return result
  where
    handleServerError :: ServerError -> Servant.Handler a 
    handleServerError err = do
      liftIO . printLog ERROR $ logMsg ("exited with" :: String) (errHTTPCode err) (errBody err)
      throwError err
    
    action = do
      lgr  <- Google.newLogger Google.Info stdout

      env  <- Google.newEnv <&>
            (Google.envLogger .~ lgr)
          . (Google.envScopes .~ FireStore.cloudPlatformScope)

      runResourceT $ Google.runGoogle env gcp

-- ============== Document store ================
toDoc :: ToDocValue a => a -> FireStore.Document
toDoc = buildDocFromValue . toValue

fromDoc :: FromDocValue a => FireStore.Document -> Either String a
fromDoc = undefined
