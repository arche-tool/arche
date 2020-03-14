{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.SubmitANG
  ( submitAngHandler 
  ) where

import Control.Lens                 ((&), (.~), (<&>), (?~))
import Control.Monad                (void)
import Control.Monad.Trans.Resource (runResourceT)
import Network.HTTP.Conduit         (RequestBody(..))
import Network.HTTP.Media.MediaType ((//))
import System.IO                    (stdout)

import qualified Network.Google           as Google
import qualified Network.Google.FireStore as FireStore
import qualified Network.Google.Storage   as Storage
import qualified Data.ByteString.Lazy     as BSL

import File.EBSD (loadEBSD, EBSDdata(ANG, CTF))

import Type.API
import Type.Storage
import Type.Store

import Util.Hash (calculateHashEBSD)

submitAngHandler :: BSL.ByteString -> IO (User)
submitAngHandler bs = do
  lgr  <- Google.newLogger Google.Info stdout

  env  <- Google.newEnv <&>
        (Google.envLogger .~ lgr)
      . (Google.envScopes .~ FireStore.cloudPlatformScope)

  hash <- runResourceT . Google.runGoogle env $ do
    let
      ebsd = either error id (loadEBSD bs)
      ebsdHash = calculateHashEBSD ebsd 
    case ebsd of
      CTF _ -> do
        saveEBSD ctfBucket ebsdHash bs
      ANG _ -> do
        saveEBSD angBucket ebsdHash bs
    return ebsdHash

  runResourceT . Google.runGoogle env $ do
    let path = "projects/apt-muse-269419/databases/(default)/documents/users/j01nabZkuzXv149VIrY8"
    doc :: FireStore.Document <- Google.send (FireStore.projectsDatabasesDocumentsGet path)
    either error return (fromDoc doc)

saveEBSD :: StorageBucket -> HashEBSD -> BSL.ByteString
        -> Google.Google '["https://www.googleapis.com/auth/cloud-platform"] ()
saveEBSD bucket (HashEBSD angHash) bs = let
    body = Google.GBody ("application" // "octet-stream") (RequestBodyLBS bs)
    vox_key = angHash
    objIns = Storage.objectsInsert (bktText bucket) Storage.object' & Storage.oiName ?~ vox_key
    in void $ Google.upload objIns body