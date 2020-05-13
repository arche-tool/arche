{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.EBSDAPI
  ( ebsdApi
  ) where

import Control.Lens                 ((&), (.~), (<&>), (?~), view)
import Control.Monad                (void, forM, when)
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Resource (runResourceT, throwM)
import Data.Maybe                   (mapMaybe)
import Network.HTTP.Conduit         (RequestBody(..))
import Network.HTTP.Media.MediaType ((//))
import System.IO                    (stdout)
import Servant.Multipart            (Mem, MultipartData, files, fdPayload, inputs)
import Servant

import qualified Network.Google           as Google
import qualified Network.Google.FireStore as FireStore
import qualified Network.Google.Storage   as Storage
import qualified Data.ByteString.Lazy     as BSL

import File.EBSD (loadEBSD, EBSDdata(ANG, CTF))

import Type.API
import Type.Storage
import Type.Store

import Util.Hash (calculateHashEBSD)
import Util.FireStore (FromDocValue, GCP, fromDoc, toDoc, runGCPWith )

-- type EBSDAPI = "ebsd" :>
--   (MultipartForm Mem (MultipartData Mem) :> Post '[JSON] [EBSD]
--   :<|>                                      Get  '[JSON] [EBSD]
--   :<|> Capture "hash" HashEBSD           :> Get  '[JSON] EBSD
--   )
ebsdApi :: User -> Server EBSDAPI
ebsdApi user = let
  post = uploadEbsdAPI user
  gets = runGCPWith (getEBSDs user)
  get  = runGCPWith . getEBSD user
  in (post :<|> gets :<|> get)

getEBSD :: User -> HashEBSD -> Google.Google GCP EBSD
getEBSD user (HashEBSD hash) = do
  let path = "projects/apt-muse-269419/databases/(default)/documents/ebsd/" <> hash
  resp <- Google.send (FireStore.projectsDatabasesDocumentsGet path)
  let ebsd = either error id (fromDoc resp)
  when (createdBy ebsd /= user) $ do
    throwM $ err404 {
      errBody = "I still haven't found what I'm looking for ... O_o"
    } 
  return ebsd

getEBSDs :: (FromDocValue a) => User -> Google.Google GCP [a]
getEBSDs user = do
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

uploadEbsdAPI :: User -> MultipartData Mem -> Handler [EBSD]
uploadEbsdAPI user = \upload -> do
  liftIO . putStrLn . show $ (inputs upload)
  forM (files upload) $ \file -> do
    let content = fdPayload file
    runGCPWith (submitEbsd user content)

submitEbsd :: User -> BSL.ByteString -> Google.Google GCP EBSD
submitEbsd user bs = do
  let
    ebsdBlob = either error id (loadEBSD bs)
    ebsdHash = calculateHashEBSD ebsdBlob 
  case ebsdBlob of
    CTF _ -> do
      saveEBSD ebsdBucket ebsdHash bs
    ANG _ -> do
      saveEBSD ebsdBucket ebsdHash bs
  let ebsd = EBSD
           { alias     = ""
           , hashEBSD  = ebsdHash
           , createdBy = user
           }
  writeEBSD ebsd 
  writePermissionEBSD user ebsdHash 
  return ebsd

writeEBSD :: EBSD -> Google.Google GCP ()
writeEBSD ebsd  = do
    let
      HashEBSD hash = hashEBSD ebsd
      path = "projects/apt-muse-269419/databases/(default)/documents/ebsd/" <> hash
    void $ Google.send (FireStore.projectsDatabasesDocumentsPatch (toDoc ebsd) path)

writePermissionEBSD :: User -> HashEBSD -> Google.Google GCP ()
writePermissionEBSD user (HashEBSD hash)  = do
    let
      db = "projects/apt-muse-269419/databases/(default)"
      path = db <> "/documents/ebsd/" <> hash
    
      fieldTxn :: FireStore.FieldTransform 
      fieldTxn = FireStore.fieldTransform &
          FireStore.ftFieldPath ?~ "permission" &
          FireStore.ftAppendMissingElements ?~ (FireStore.arrayValue & FireStore.avValues .~ [val])
        where val = FireStore.value & FireStore.vStringValue ?~ (email user) 
      
      markLastUpdate :: FireStore.FieldTransform 
      markLastUpdate = FireStore.fieldTransform &
          FireStore.ftFieldPath ?~ "lastUpdate" &
          FireStore.ftSetToServerValue ?~ FireStore.RequestTime

      docTxn :: FireStore.DocumentTransform
      docTxn = FireStore.documentTransform &
          FireStore.dtFieldTransforms .~ [fieldTxn, markLastUpdate] &
          FireStore.dtDocument ?~ path
    
      write :: FireStore.Write
      write = FireStore.write & FireStore.wTransform ?~ docTxn
    
      commitReq :: FireStore.CommitRequest
      commitReq = FireStore.commitRequest & FireStore.crWrites .~ [write]

    void $ Google.send (FireStore.projectsDatabasesDocumentsCommit db commitReq)

saveEBSD :: StorageBucket -> HashEBSD -> BSL.ByteString -> Google.Google GCP ()
saveEBSD bucket (HashEBSD angHash) bs = let
    body = Google.GBody ("application" // "octet-stream") (RequestBodyLBS bs)
    vox_key = angHash
    objIns = Storage.objectsInsert (bktText bucket) Storage.object' & Storage.oiName ?~ vox_key
    in void $ Google.upload objIns body
  
writeUser :: User -> Google.Google GCP ()
writeUser user = do
    let path = "projects/apt-muse-269419/databases/(default)/documents/users/" <> email user
    void $ Google.send (FireStore.projectsDatabasesDocumentsPatch (toDoc user) path)

writeNewUser :: User -> Google.Google GCP ()
writeNewUser user = do
    let path = "projects/apt-muse-269419/databases/(default)/documents"
    void $ Google.send (FireStore.projectsDatabasesDocumentsCreateDocument path "users" (toDoc user))
