{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeOperators       #-}

module Handler.ArcheAPI
  ( archeApi
  ) where

import Control.Lens                 ((&), (.~), (?~), view)
import Control.Monad                (void)
import Control.Monad.Trans.Class    (lift)
import Control.Monad.Trans.Resource (liftResourceT)
import Data.Binary.Builder          (toLazyByteString)
import Data.Conduit                 (runConduit, (.|))
import Data.Maybe                   (mapMaybe)
import Data.Text                    (Text)
import Network.HTTP.Client          (method, requestBody, getUri)
import Network.HTTP.Conduit         (RequestBody(..))
import Network.HTTP.Media.MediaType ((//))

import qualified Data.ByteString.Base64    as B64
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.Conduit.Binary       as Conduit
import qualified Data.HashMap.Strict       as HM
import qualified Data.Text                 as T
import qualified Network.Google            as Google
import qualified Network.Google.FireStore  as FireStore
import qualified Network.Google.Storage    as Storage
import qualified Network.Google.CloudTasks as Tasks
import Servant

import qualified Arche.Strategy.GomesGraph as GG
import qualified Arche.Strategy.ORFitAll   as OR

import Type.API
import Type.Storage
import Type.Store
import Util.FireStore
import Util.Hash
import Util.Logger    (logGGInfo, logMsg)
import Handler.ORAPI  (getOR)

import qualified Util.Auth   as Auth
import qualified Util.Client as Client

-- ==<< Reconstruction handling >>==
-- POST /ebsd/hash/{ebsd_hash}/orfit/hash/{or_hash}/arche ArcheCfg Arche
-- GET  /ebsd/hash/{ebsd_hash}/orfit/hash/{or_hash}/arche [Arche]
-- GET  /ebsd/hash/{ebsd_hash}/orfit/hash/{or_hash}/arche/hash/{arche_hash} Arche
archeApi :: Auth.BearerToken -> User -> Server ArcheAPI
archeApi tk user = \hashebsd hashor ->
       (runGCPWith $ getArches hashebsd hashor)
  :<|> (runGCPWith . getArche user hashebsd hashor)
  :<|> (\cfg -> runGCPWith $ runArcheHandler user hashebsd hashor cfg)
  :<|> (\cfg -> runGCPWith $ runAsyncArcheHandler tk hashebsd hashor cfg)


fetchEbsdData :: HashEBSD -> Google.Google GCP BSL.ByteString
fetchEbsdData (HashEBSD hash) = do
  stream <- Google.download (Storage.objectsGet (bktName ebsdBucket) hash)
  liftResourceT (runConduit (stream .| Conduit.sinkLbs))

runArcheHandler :: User -> HashEBSD -> HashOR -> ArcheCfg -> Google.Google GCP Arche
runArcheHandler user hashebsd hashor cfg = do
  ang      <- fetchEbsdData hashebsd
  orResult <- getOR user hashebsd hashor 

  let
    action = do
      bs <- GG.renderImage
      lift $ savePngImage imageBucket hashebsd bs
      return ()
     
  !finalState <- GG.processEBSD (webCfgToCLICfg cfg orResult) ang action

  let arche = Arche
            { hashArche = calculateHashArche cfg
            , cfgArche  = cfg
            }
  
  writeArche hashebsd hashor arche 

  return arche

writeArche :: HashEBSD -> HashOR -> Arche -> Google.Google GCP ()
writeArche (HashEBSD hashE) (HashOR hashO) value  = do
    let
      HashArche hashA = hashArche value
      path = "projects/apt-muse-269419/databases/(default)/documents/ebsd/" <> hashE <> "/or/" <> hashO <> "/arche/" <> hashA
    void $ Google.send (FireStore.projectsDatabasesDocumentsPatch (toDoc value) path)

getArche :: User -> HashEBSD -> HashOR -> HashArche -> Google.Google GCP Arche
getArche user (HashEBSD hashE) (HashOR hashO) (HashArche hashA) = do
  logGGInfo $ logMsg ("Retriving OR" :: String) hashO ("for user" :: String) (id_number user)
  let path = "projects/apt-muse-269419/databases/(default)/documents/ebsd/" <> hashE <> "/or/" <> hashO <> "/arche/" <> hashA
  resp <- Google.send (FireStore.projectsDatabasesDocumentsGet path)
  let orDoc = either error id (fromDoc resp)
  return orDoc

getArches :: (FromDocValue a) => HashEBSD -> HashOR -> Google.Google GCP [a]
getArches (HashEBSD hashE) (HashOR hashO) = do
  let
    db = "projects/apt-muse-269419/databases/(default)/documents/ebsd/" <> hashE <> "/or/" <> hashO
    
    query :: FireStore.StructuredQuery
    query = let
      from = FireStore.collectionSelector &
        FireStore.csCollectionId ?~ "arche" &
        FireStore.csAllDescendants ?~ False
      in FireStore.structuredQuery
        & FireStore.sqFrom .~ [from] 
    
    commitReq :: FireStore.RunQueryRequest
    commitReq = FireStore.runQueryRequest & FireStore.rqrStructuredQuery ?~ query

  logGGInfo $ logMsg ("Retriving ORs for EBSD" :: String) hashE
  resp <- Google.send (FireStore.projectsDatabasesDocumentsRunQuery db commitReq)
  return . mapMaybe (fmap (either error id . fromDoc) . view FireStore.rDocument) $ resp

webCfgToCLICfg :: ArcheCfg -> OR -> GG.Cfg
webCfgToCLICfg (ArcheCfg {..}) orResult = let
  orValue = OR.orValue . OR.orientationRelationship . resultOR $ orResult
  in GG.Cfg
  { GG.misoAngle              = misoAngle
  , GG.useExternalMCL         = False
  , GG.excludeFloatingGrains  = excludeFloatingGrains
  , GG.refinementSteps        = refinementSteps
  , GG.initClusterFactor      = initClusterFactor
  , GG.stepClusterFactor      = stepClusterFactor
  , GG.badAngle               = badAngle
  , GG.withOR                 = orValue
  , GG.parentPhaseID          = parentPhaseID
  , GG.outputANGMap           = False
  , GG.outputCTFMap           = False
}


savePngImage :: StorageBucket -> HashEBSD -> BSL.ByteString -> Google.Google GCP ()
savePngImage bucket (HashEBSD ebsdHash) bs = let
  body = Google.GBody ("application" // "octet-stream") (RequestBodyLBS bs)
  vox_key = ebsdHash
  objIns = Storage.objectsInsert (bktName bucket) Storage.object' & Storage.oiName ?~ vox_key
  in do
    logGGInfo $ logMsg ("Saving image blob with hash" :: String) ebsdHash
    void $ Google.upload objIns body
    logGGInfo $ logMsg ("Saved image blob with hash" :: String) ebsdHash

runAsyncArcheHandler :: Auth.BearerToken -> HashEBSD -> HashOR -> ArcheCfg -> Google.Google GCP Text
runAsyncArcheHandler tk hashE hashO archeCfg = do
  httpReq <- either fail return $ createTask tk hashE hashO archeCfg
  let 
    parent = "projects/apt-muse-269419/locations/europe-west1/queues/reconstruction-queue"
    task = Tasks.task & Tasks.tHTTPRequest ?~ httpReq
    taskReq = Tasks.createTaskRequest & Tasks.ctrTask ?~ task
  resp <- Google.send (Tasks.projectsLocationsQueuesTasksCreate parent taskReq)
  name <- maybe (fail "Async task needs a name") return $ view Tasks.tName resp
  logGGInfo $ logMsg ("Sending async task " :: String) name
  return name
  
createTask :: Auth.BearerToken -> HashEBSD -> HashOR -> ArcheCfg -> Either String Tasks.HTTPRequest
createTask tk hashE hashO archeCfg = let
  archeapi = (Client.archeApiClient Client.mkApiClient) hashE hashO
  toBS req = case requestBody req of
    RequestBodyLBS lbs     -> return $ BSL.toStrict lbs
    RequestBodyBS   bs     -> return bs
    RequestBodyBuilder _ b -> return . BSL.toStrict . toLazyByteString $ b
    _                      -> fail "Can not build request body."
  toMethod req = case method req of
    "POST"    -> Tasks.HTTPRHTTPMPost'
    "GET"     -> Tasks.HTTPRHTTPMGet'
    "HEAD"    -> Tasks.HTTPRHTTPMHead'
    "PUT"     -> Tasks.HTTPRHTTPMPut'
    "DELETE"  -> Tasks.HTTPRHTTPMDelete'
    "PATCH"   -> Tasks.HTTPRHTTPMPatch'
    "OPTIONS" -> Tasks.HTTPRHTTPMOptions
    _         -> Tasks.HTTPRHTTPMHTTPMethodUnspecified
  headers = HM.fromList [
    ("Authorization", "Bearer " <> Auth.bearerToken tk),
    ("Content-Type", "application/json")
    ]
  in do
    req  <- Client.getRequestOnBaseUrl "https://compute.api.arche.dev" ((Client.postArche archeapi) archeCfg)
    body <- toBS req
    return $
      Tasks.hTTPRequest
        & Tasks.httprURL ?~ (T.pack . show . getUri $ req)
        & Tasks.httprBody ?~ (B64.encode body)
        & Tasks.httprHeaders ?~ (Tasks.hTTPRequestHeaders headers)
        & Tasks.httprHTTPMethod ?~ (toMethod req)