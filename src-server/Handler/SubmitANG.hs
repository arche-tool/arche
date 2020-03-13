{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.SubmitANG
  ( 
  ) where

import Control.Lens                 ((&), (.~), (^.), (<&>), (?~))
import Control.Monad                (void)
import Control.Monad.Trans.Resource (liftResourceT, runResourceT)
import Control.Monad.IO.Class       (liftIO)
import Data.Conduit                 (runConduit, (.|))
import Data.Text.Lazy               (Text)
import Network.HTTP.Conduit         (RequestBody(..))
import Network.HTTP.Media.MediaType ((//))
import System.IO                    (stdout)
import Data.Text.Lazy.Encoding      (decodeUtf8)

import qualified Data.Conduit.Binary      as Conduit
import qualified Network.Google           as Google
import qualified Network.Google.FireStore as FireStore
import Servant

import Texture.Orientation      (Deg(..))

import Type.API
import Type.Store

submitHandler :: IO (User)
submitHandler = do
  lgr  <- Google.newLogger Google.Info stdout

  env  <- Google.newEnv <&>
        (Google.envLogger .~ lgr)
      . (Google.envScopes .~ FireStore.cloudPlatformScope)

  runResourceT . Google.runGoogle env $ do
    let path = "projects/apt-muse-269419/databases/(default)/documents/users/j01nabZkuzXv149VIrY8"
    doc :: FireStore.Document <- Google.send (FireStore.projectsDatabasesDocumentsGet path)
    either error return (fromDoc doc)
