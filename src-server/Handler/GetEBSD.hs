{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.GetEBSD
  ( getEBSDsAPI
  , listUserEBSDs 
  ) where

import Control.Lens                 ((&), (.~), (<&>), (?~), view)
import Control.Monad                (void, forM_)
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Maybe                   (mapMaybe)
import Network.HTTP.Conduit         (RequestBody(..))
import Network.HTTP.Media.MediaType ((//))
import System.IO                    (stdout)
import Servant

import qualified Network.Google           as Google
import qualified Network.Google.FireStore as FireStore
import qualified Network.Google.Storage   as Storage
import qualified Data.ByteString.Lazy     as BSL

import File.EBSD (loadEBSD, EBSDdata(ANG, CTF))

import Type.API
import Type.Storage
import Type.Store

import Util.FireStore (FromDocument, fromDoc)

getEBSDsAPI :: Server UploadEbsdAPI
getEBSDsAPI = let
  user = User "ze@gmail.com" (Just "zeze")
  in \upload -> do
    liftIO $ listUserEBSDs user
    return NoContent

listUserEBSDs :: User -> IO [User]
listUserEBSDs user = do
  lgr  <- Google.newLogger Google.Info stdout

  env  <- Google.newEnv <&>
        (Google.envLogger .~ lgr)
      . (Google.envScopes .~ FireStore.cloudPlatformScope)

  runResourceT . Google.runGoogle env $ do
    findReadableEBSDs user

type GCP = '["https://www.googleapis.com/auth/cloud-platform"]

findReadableEBSDs :: (FromDocument a) => User -> Google.Google GCP [a]
findReadableEBSDs user = do
  let
    db = "projects/apt-muse-269419/databases/(default)/documents"
    
    query :: FireStore.StructuredQuery
    query = let
      from = FireStore.collectionSelector &
        FireStore.csCollectionId ?~ "ebsd" &
        FireStore.csAllDescendants ?~ False
      lastUpdateField = FireStore.fieldReference & FireStore.frFieldPath ?~ "lastUpdate"
      permissionField = FireStore.fieldReference & FireStore.frFieldPath ?~ "permission"
      permissionValue = FireStore.value & FireStore.vStringValue ?~ (email user)
      where' = FireStore.filter' & FireStore.fFieldFilter ?~ (
        FireStore.fieldFilter
          & FireStore.ffField ?~ permissionField
          & FireStore.ffValue ?~ permissionValue
          & FireStore.ffOp ?~ FireStore.FFOArrayContains
        )
      order = FireStore.order
        & FireStore.oDirection ?~ FireStore.ODDescending
        & FireStore.oField ?~ lastUpdateField
      in FireStore.structuredQuery
        & FireStore.sqFrom .~ [from] 
        & FireStore.sqWhere ?~ where'
        & FireStore.sqOrderBy .~ [order]
    
    commitReq :: FireStore.RunQueryRequest
    commitReq = FireStore.runQueryRequest & FireStore.rqrStructuredQuery ?~ query

  resp <- Google.send (FireStore.projectsDatabasesDocumentsRunQuery db commitReq)
  return . mapMaybe (fmap (either error id . fromDoc) . view FireStore.rDocument) $ resp
