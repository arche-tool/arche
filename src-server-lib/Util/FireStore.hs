{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
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

-- ============== Document store ================
toDoc :: ToDocValue a => a -> FireStore.Document
toDoc = buildDocFromValue . toValue

fromDoc :: FromDocValue a => FireStore.Document -> Either String a
fromDoc = undefined
