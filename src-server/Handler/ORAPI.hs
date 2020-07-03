{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}

module Handler.ORAPI
  ( orApi
  ) where

import Control.Lens                 ((&), (?~))
import Control.Monad                (void)
import Control.Monad.Trans.Resource (liftResourceT)
import Control.Monad.IO.Class       (liftIO)
import Data.Conduit                 (runConduit, (.|))
import Data.Text                    (Text)
import Network.HTTP.Conduit         (RequestBody(..))
import Network.HTTP.Media.MediaType ((//))

import qualified Data.Conduit.Binary      as Conduit
import qualified Network.Google           as Google
import qualified Network.Google.FireStore as FireStore
import qualified Network.Google.Storage   as Storage
import Servant

import Data.VTK (renderUniVTK)

import qualified Arche.Strategy.ORFitAll as OR

import Type.API
import Type.Storage
import Type.Store
import Util.FireStore
import Util.Hash

--type ORAPI = "ebsd" :> Capture "hash" HashEBSD :> "orfit" :>
--  (                              Get  '[JSON] [OR]
--  :<|> Capture "hash" HashOR  :> Get  '[JSON] OR
--  :<|> ReqBody '[JSON] OR.Cfg :> Post '[JSON] NoContent
--  )

orApi :: User -> Server ORAPI
orApi _ = \hashebsd ->
       (return [])
  :<|> (\_ -> return undefined)
  :<|> (\cfg -> runGCPWith $ orFitHandler hashebsd cfg "ebsd" )


orFitHandler :: HashEBSD -> OR.Cfg -> Text -> Google.Google GCP OR
orFitHandler hashebsd@(HashEBSD hash) cfg bucket = do
  stream <- Google.download (Storage.objectsGet bucket hash)
  ang    <- liftResourceT (runConduit (stream .| Conduit.sinkLbs))

  -- Force strictness on OR calculation otherwise the data
  --upload bellow can timeout while awaiting for calculation.
  (!orEval, vtk) <- liftIO $ OR.processEBSD cfg ang

  let
    body = Google.GBody ("application" // "octet-stream") (RequestBodyLBS $ renderUniVTK True vtk)
    vox_key = hash <> ".vtk"
  
  void $ Google.upload (Storage.objectsInsert bucket Storage.object' & Storage.oiName ?~ vox_key) body

  let orship = OR
         { hashOR   = calculateHashOR cfg
         , cfgOR    = cfg
         , resultOR = orEval
         }
  
  writeOR hashebsd orship 

  return orship

writeOR :: HashEBSD -> OR -> Google.Google GCP ()
writeOR (HashEBSD hashE) or  = do
    let
      HashOR hashO = hashOR or
      path = "projects/apt-muse-269419/databases/(default)/documents/ebsd/" <> hashE <> "/or/" <> hashO
    void $ Google.send (FireStore.projectsDatabasesDocumentsPatch (toDoc or) path)
