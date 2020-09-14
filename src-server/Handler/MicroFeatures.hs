{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.MicroFeatures
  ( microFeatureHandler 
  ) where

import Control.Lens                 ((&), (.~), (^.), (<&>), (?~))
import Control.Monad                (void)
import Control.Monad.Trans.Resource (liftResourceT, runResourceT)
import Control.Monad.IO.Class       (liftIO)
import Data.Conduit                 (runConduit, (.|))
import Data.Text                    (Text)
import Network.HTTP.Conduit         (RequestBody(..))
import Network.HTTP.Media.MediaType ((//))
import System.IO                    (stdout)
import Servant

import qualified Data.Conduit.Binary      as Conduit
import qualified Network.Google           as Google
import qualified Network.Google.FireStore as FireStore
import qualified Network.Google.Storage   as Storage
import qualified Data.ByteString.Lazy     as BSL

import Texture.Orientation  (Deg(..))
import Data.VTK             (VTK, renderUniVTK)
import qualified Hammer.VTK           as VTK
import qualified Arche.Strategy.Graph as GR

import Type.API
import Type.Store

microFeatureHandler :: GR.Cfg -> BSL.ByteString -> IO (User)
microFeatureHandler cfg bs = do
  lgr  <- Google.newLogger Google.Info stdout

  env  <- Google.newEnv <&>
        (Google.envLogger .~ lgr)
      . (Google.envScopes .~ FireStore.cloudPlatformScope)

  runResourceT . Google.runGoogle env $ do
    case GR.processEBSD cfg bs of
      Left err -> error err
      Right (gids, micro, attrs) -> do
        let angHash = HashEBSD "dddd" 
        saveVTK voxelBucket  angHash ".vtr" $ VTK.renderVoxBoxVTK      gids attrs
        saveVTK facesBucket  angHash ".vtu" $ VTK.renderMicroFacesVTK  gids micro
        saveVTK edgesBucket  angHash ".vtu" $ VTK.renderMicroEdgesVTK  gids micro
        saveVTK vertexBucket angHash ".vtu" $ VTK.renderMicroVertexVTK gids micro

  runResourceT . Google.runGoogle env $ do
    let path = "projects/apt-muse-269419/databases/(default)/documents/users/j01nabZkuzXv149VIrY8"
    doc :: FireStore.Document <- Google.send (FireStore.projectsDatabasesDocumentsGet path)
    either error return (fromDoc doc)

saveVTK :: (VTK.RenderElemVTK a) => StorageBucket -> HashEBSD -> Text -> VTK a
        -> Google.Google '["https://www.googleapis.com/auth/cloud-platform"] ()
saveVTK bucket (HashEBSD angHash) extension vtk = let
    body = Google.GBody ("application" // "octet-stream") (RequestBodyLBS $ renderUniVTK True vtk)
    vox_key = angHash <> extension
    objIns = Storage.objectsInsert (bktName bucket) Storage.object' & Storage.oiName ?~ vox_key
    in void $ Google.upload objIns body