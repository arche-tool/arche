{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
module Util.FireStore
    ( GCP
    , runGCPWith
    , toDoc
    , fromDoc
    , logGoogle
    , logInfo
    , logError
    , module Util.FireStore.Value
    , module Util.FireStore.Document
    ) where

import Control.Lens                 ((.~), (<&>), view)
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Representable.Reader (reader)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Binary.Builder          (putStringUtf8)
import System.IO                    (stdout)
import Servant                      (Handler)

import qualified Network.Google           as Google
import qualified Network.Google.FireStore as FireStore

import Util.FireStore.Document
import Util.FireStore.Value

-- ============== Google Cloud ================
type GCP = '["https://www.googleapis.com/auth/cloud-platform"]

runGCPWith :: Google.Google GCP a -> Handler a
runGCPWith gcp = liftIO $ do
  lgr  <- Google.newLogger Google.Info stdout

  env  <- Google.newEnv <&>
        (Google.envLogger .~ lgr)
      . (Google.envScopes .~ FireStore.cloudPlatformScope)

  runResourceT $ Google.runGoogle env gcp

logGoogle :: Show a => Google.LogLevel -> a -> Google.Google s ()
logGoogle level x = do
  logger <- reader (view Google.envLogger)
  liftIO $ logger level (putStringUtf8 . show $ x)

logInfo :: Show a => a -> Google.Google s ()
logInfo = logGoogle Google.Info

logError :: Show a => a -> Google.Google s ()
logError = logGoogle Google.Error

-- ============== Document store ================
toDoc :: ToDocValue a => a -> FireStore.Document
toDoc = buildDocFromValue . toValue

fromDoc :: FromDocValue a => FireStore.Document -> Either String a
fromDoc = undefined
