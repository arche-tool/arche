{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.EBSDAPI
  ( ebsdApi
  ) where

import Control.Lens                 ((&), (.~), (?~), view)
import Control.Monad                (void, forM, when)
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Resource (throwM)
import Data.Maybe                   (mapMaybe)
import Data.String                  (fromString)
import Network.HTTP.Conduit         (RequestBody(..))
import Network.HTTP.Media.MediaType ((//))
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
import Util.FireStore (FromDocValue, GCP, fromDoc, toDoc, runGCPWith)
import Util.Logger (logGGInfo, logMsg)

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
    throwM $ err401 {
      errBody = "You don't have access to this resource ... O_o"
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
      permissionValue = FireStore.value & FireStore.vStringValue ?~ (id_number user)
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
  forM (files upload) $ \file -> do
    let content = fdPayload file
    runGCPWith (submitEbsd user content)

submitEbsd :: User -> BSL.ByteString -> Google.Google GCP EBSD
submitEbsd user bs = do
  ebsdBlob <- either shout400 return (loadEBSD bs)
  let ebsdHash = calculateHashEBSD ebsdBlob 
  logGGInfo $ logMsg ("Submiting EBSD map with hash" :: String) ebsdHash
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
  where
    shout400 msg = throwM $ err400 {errBody = fromString msg} 

writeEBSD :: EBSD -> Google.Google GCP ()
writeEBSD ebsd  = let
  HashEBSD hash = hashEBSD ebsd
  path = "projects/apt-muse-269419/databases/(default)/documents/ebsd/" <> hash
  in do
    logGGInfo $ logMsg ("Saving EBSD metadata with hash" :: String) hash
    void $ Google.send (FireStore.projectsDatabasesDocumentsPatch (toDoc ebsd) path)

writePermissionEBSD :: User -> HashEBSD -> Google.Google GCP ()
writePermissionEBSD user (HashEBSD hash)  = let
  db = "projects/apt-muse-269419/databases/(default)"
  path = db <> "/documents/ebsd/" <> hash
    
  fieldTxn :: FireStore.FieldTransform 
  fieldTxn = FireStore.fieldTransform &
      FireStore.ftFieldPath ?~ "permission" &
      FireStore.ftAppendMissingElements ?~ (FireStore.arrayValue & FireStore.avValues .~ [val])
    where val = FireStore.value & FireStore.vStringValue ?~ (id_number user) 
      
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

  in do
    logGGInfo $ logMsg ("Setting permission on EBSD with hash" :: String) hash
    void $ Google.send (FireStore.projectsDatabasesDocumentsCommit db commitReq)

saveEBSD :: StorageBucket -> HashEBSD -> BSL.ByteString -> Google.Google GCP ()
saveEBSD bucket (HashEBSD ebsdHash) bs = let
  body = Google.GBody ("application" // "octet-stream") (RequestBodyLBS bs)
  vox_key = ebsdHash
  objIns = Storage.objectsInsert (bktText bucket) Storage.object' & Storage.oiName ?~ vox_key
  in do
    logGGInfo $ logMsg ("Saving EBSD blob with hash" :: String) ebsdHash
    void $ Google.upload objIns body
    logGGInfo $ logMsg ("Saved EBSD blob with hash" :: String) ebsdHash
  
writeUser :: User -> Google.Google GCP ()
writeUser user = do
    let path = "projects/apt-muse-269419/databases/(default)/documents/users/" <> id_number user
    void $ Google.send (FireStore.projectsDatabasesDocumentsPatch (toDoc user) path)

writeNewUser :: User -> Google.Google GCP ()
writeNewUser user = do
    let path = "projects/apt-muse-269419/databases/(default)/documents"
    void $ Google.send (FireStore.projectsDatabasesDocumentsCreateDocument path "users" (toDoc user))
