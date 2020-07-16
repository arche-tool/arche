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
import Control.Monad.Trans.Resource (liftResourceT)
import Control.Monad.IO.Class       (liftIO)
import Data.Conduit                 (runConduit, (.|))
import Data.Maybe                   (mapMaybe)
import Data.Text                    (Text)
import Network.HTTP.Conduit         (RequestBody(..))
import Network.HTTP.Media.MediaType ((//))

import qualified Data.Conduit.Binary      as Conduit
import qualified Network.Google           as Google
import qualified Network.Google.FireStore as FireStore
import qualified Network.Google.Storage   as Storage
import Servant

import Data.VTK (renderUniVTK)

import qualified Arche.Strategy.GomesGraph as GG
import qualified Arche.Strategy.ORFitAll   as OR
import qualified Texture.Orientation       as TO

import Type.API
import Type.Storage
import Type.Store
import Util.FireStore
import Util.Hash
import Util.Logger    (logGGInfo, logMsg)
import Handler.ORAPI  (getOR)

-- ==<< Reconstruction handling >>==
-- POST /ebsd/hash/{ebsd_hash}/orfit/hash/{or_hash}/arche ArcheCfg Arche
-- GET  /ebsd/hash/{ebsd_hash}/orfit/hash/{or_hash}/arche [Arche]
-- GET  /ebsd/hash/{ebsd_hash}/orfit/hash/{or_hash}/arche/hash/{arche_hash} Arche
archeApi :: User -> Server ArcheAPI
archeApi user = \hashebsd hashor ->
       (runGCPWith $ getArches hashebsd hashor)
  :<|> (runGCPWith . getArche user hashebsd hashor)
  :<|> (\cfg -> runGCPWith $ runArcheHandler user hashebsd hashor cfg "arche" )


runArcheHandler :: User -> HashEBSD -> HashOR -> ArcheCfg -> Text -> Google.Google GCP Arche
runArcheHandler user hashebsd@(HashEBSD hash) hashor cfg bucket = do
  stream <- Google.download (Storage.objectsGet bucket hash)
  ang    <- liftResourceT (runConduit (stream .| Conduit.sinkLbs))

  orResult <- getOR user hashebsd hashor 

  let
    action = return ()
     
  !finalState <- liftIO $ GG.processEBSD (webCfgToCLICfg cfg orResult) ang action

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