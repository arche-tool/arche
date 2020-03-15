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
import Util.FireStore (toDoc)

submitAngHandler :: User -> BSL.ByteString -> IO ()
submitAngHandler user bs = do
  lgr  <- Google.newLogger Google.Info stdout

  env  <- Google.newEnv <&>
        (Google.envLogger .~ lgr)
      . (Google.envScopes .~ FireStore.cloudPlatformScope)

  runResourceT . Google.runGoogle env $ do
    let
      ebsd = either error id (loadEBSD bs)
      ebsdHash = calculateHashEBSD ebsd 
    case ebsd of
      CTF _ -> do
        saveEBSD ebsdBucket ebsdHash bs
      ANG _ -> do
        saveEBSD ebsdBucket ebsdHash bs
    writeHashEBSD user ebsdHash 

type GCP = '["https://www.googleapis.com/auth/cloud-platform"]

writeUser :: User -> Google.Google GCP ()
writeUser user = do
    let path = "projects/apt-muse-269419/databases/(default)/documents/users/" <> email user
    void $ Google.send (FireStore.projectsDatabasesDocumentsPatch (toDoc user) path)

writeHashEBSD :: User -> HashEBSD -> Google.Google GCP ()
writeHashEBSD user (HashEBSD hash)  = do
    let path = "projects/apt-muse-269419/databases/(default)/documents/ebsd/" <> hash
    void $ Google.send (FireStore.projectsDatabasesDocumentsPatch (toDoc user) path)

writeNewUser :: User -> Google.Google GCP ()
writeNewUser user = do
    let path = "projects/apt-muse-269419/databases/(default)/documents"
    void $ Google.send (FireStore.projectsDatabasesDocumentsCreateDocument path "users" (toDoc user))

saveEBSD :: StorageBucket -> HashEBSD -> BSL.ByteString -> Google.Google GCP ()
saveEBSD bucket (HashEBSD angHash) bs = let
    body = Google.GBody ("application" // "octet-stream") (RequestBodyLBS bs)
    vox_key = angHash
    objIns = Storage.objectsInsert (bktText bucket) Storage.object' & Storage.oiName ?~ vox_key
    in void $ Google.upload objIns body