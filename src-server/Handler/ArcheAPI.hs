{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeOperators       #-}

module Handler.ArcheAPI
  ( archeApi
  ) where

import Control.Lens                 ((&), (.~), (?~), view)
import Control.Monad                (void)
import Control.Monad.RWS            (gets)
import Control.Monad.Trans.Class    (lift)
import Control.Monad.Trans.Resource (liftResourceT)
import Data.Conduit                 (runConduit, (.|))
import Data.Maybe                   (mapMaybe)
import Network.HTTP.Conduit         (RequestBody(..))
import Network.HTTP.Media.MediaType ((//))

import qualified Data.ByteString.Lazy      as BSL
import qualified Data.Conduit.Binary       as Conduit
import qualified Network.Google            as Google
import qualified Network.Google.FireStore  as FireStore
import qualified Network.Google.Storage    as Storage
import Servant

import qualified Arche.Strategy.GomesGraph as GG
import qualified Arche.Strategy.ORFitAll   as OR

import Type.API
import Type.Storage
import Type.Store
import Util.FireStore
import Util.Hash
import Util.Storage
import Util.Logger    (logGGInfo, logMsg)
import Handler.ORAPI  (getOR)

import qualified Util.Auth   as Auth
import qualified Util.Client as Client
import qualified Util.Tasks  as SelfTasks

-- ==<< Reconstruction handling >>==
-- POST /ebsd/hash/{ebsd_hash}/orfit/hash/{or_hash}/arche ArcheCfg Arche
-- GET  /ebsd/hash/{ebsd_hash}/orfit/hash/{or_hash}/arche [Arche]
-- GET  /ebsd/hash/{ebsd_hash}/orfit/hash/{or_hash}/arche/hash/{arche_hash} Arche
archeApi :: Auth.BearerToken -> User -> Server ArcheAPI
archeApi tk user = \hashE hashO ->
       (runGCPWith $ addHeader private15sCache <$> getArches hashE hashO)
  :<|> (runGCPWith . getArche user hashE hashO)
  :<|> (\cfg -> runGCPWith $ runArcheHandler user hashE hashO cfg)
  :<|> (\cfg -> runGCPWith $ runAsyncArcheHandler tk hashE hashO cfg)
  where
    private15sCache = "private, max-age=15, s-maxage=15" :: String


fetchEbsdData :: HashEBSD -> Google.Google GCP BSL.ByteString
fetchEbsdData (HashEBSD hash) = do
  stream <- Google.download (Storage.objectsGet (bktName ebsdBucket) hash)
  liftResourceT (runConduit (stream .| Conduit.sinkLbs))

runArcheHandler :: User -> HashEBSD -> HashOR -> ArcheCfg -> Google.Google GCP (Arche StoragePublicLink)
runArcheHandler user hashE hashO cfg = do
  ang      <- fetchEbsdData hashE
  orResult <- getOR user hashE hashO 

  let
    hashA = calculateHashArche cfg
    action = do
      factor <- gets GG.mclFactor
      let
        hashIPF    = calculateHashResult hashE hashO hashA (show factor ++ "IPF")
        hashAvgErr = calculateHashResult hashE hashO hashA (show factor ++ "AvgError")
      
      !bsIPF <- GG.renderIPFImage
      objIPF <- lift $ savePngImage imageBucket hashIPF bsIPF

      !bsAE <- GG.renderAvgParentError
      objAE <- lift $ savePngImage imageBucket hashAvgErr bsAE

      return $ ArcheResult
        { mclFactor = factor
        , parentIPF = objIPF
        , errorMap  = objAE
        }
     
  !results <- GG.processEBSD (webCfgToCLICfg cfg orResult) ang action

  let arche = Arche
            { hashArche = hashA 
            , cfgArche  = cfg
            , results   = results
            }
  
  writeArche hashE hashO arche 

  return $ getResultLink arche

writeArche :: HashEBSD -> HashOR -> Arche StorageObject -> Google.Google GCP ()
writeArche (HashEBSD hashE) (HashOR hashO) value  = do
    let
      HashArche hashA = hashArche value
      path = "projects/apt-muse-269419/databases/(default)/documents/ebsd/" <> hashE <> "/or/" <> hashO <> "/arche/" <> hashA
    void $ Google.send (FireStore.projectsDatabasesDocumentsPatch (toDoc value) path)

getArche :: User -> HashEBSD -> HashOR -> HashArche -> Google.Google GCP (Arche StoragePublicLink)
getArche user (HashEBSD hashE) (HashOR hashO) (HashArche hashA) = do
  logGGInfo $ logMsg ("Retriving OR" :: String) hashO ("for user" :: String) (id_number user)
  let path = "projects/apt-muse-269419/databases/(default)/documents/ebsd/" <> hashE <> "/or/" <> hashO <> "/arche/" <> hashA
  resp <- Google.send (FireStore.projectsDatabasesDocumentsGet path)
  let orDoc = either error id (fromDoc resp)
  return $ getResultLink orDoc

getArches :: HashEBSD -> HashOR -> Google.Google GCP [Arche StoragePublicLink]
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
  return . mapMaybe (fmap (either error getResultLink . fromDoc) . view FireStore.rDocument) $ resp

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
  , GG.productPhase           = productPhase
  , GG.parentPhase            = parentPhase
  , GG.outputANGMap           = False
  , GG.outputCTFMap           = False
}


savePngImage :: StorageBucket -> HashResult -> BSL.ByteString -> Google.Google GCP StorageObject
savePngImage bucket (HashResult ebsdR) bs = let
  body = Google.GBody ("application" // "octet-stream") (RequestBodyLBS bs)
  obj = StorageObject
    { objName = ebsdR
    , objExtension = Just "png"
    , objBucket = bucket
    }
  objIns = createObjectInsertion obj
  in do
    logGGInfo $ logMsg ("Saving image blob with hash" :: String) ebsdR
    void $ Google.upload objIns body
    logGGInfo $ logMsg ("Saved image blob with hash" :: String) ebsdR
    return obj

runAsyncArcheHandler :: Auth.BearerToken -> HashEBSD -> HashOR -> ArcheCfg -> Google.Google GCP HashArche
runAsyncArcheHandler tk hashE hashO archeCfg = let
  archeapi = (Client.archeApiClient Client.mkApiClient) hashE hashO
  hashA    = calculateHashArche archeCfg
  in do
    _ <- SelfTasks.submitSelfTask reconstructionQueue tk ((Client.postArche archeapi) archeCfg)
    return hashA

getResultLink :: Arche StorageObject -> Arche StoragePublicLink
getResultLink results = fmap getPublicLink results