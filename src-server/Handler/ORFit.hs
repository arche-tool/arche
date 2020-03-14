{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}

module Handler.ORFit
  ( orFitAPI
  , orFitHandler
  ) where

import Control.Lens                 ((&), (.~), (<&>), (?~))
import Control.Monad                (void)
import Control.Monad.Trans.Resource (liftResourceT, runResourceT)
import Control.Monad.IO.Class       (liftIO)
import Data.Conduit                 (runConduit, (.|))
import Data.Text                    (Text)
import Network.HTTP.Conduit         (RequestBody(..))
import Network.HTTP.Media.MediaType ((//))
import System.IO                    (stdout)

import qualified Data.Conduit.Binary    as Conduit
import qualified Network.Google         as Google
import qualified Network.Google.Storage as Storage
import Servant

import qualified Arche.Strategy.ORFitAll as OR
import Arche.Strategy.ORFitAll  (OREvaluation)
import Data.VTK                 (renderUniVTK)
import Texture.Orientation      (Deg(..))

import Type.API
import Type.Storage

orFitAPI :: Server ORFitAPI
orFitAPI = let
  cfg = OR.Cfg  { OR.misoAngle    = Deg 5
                , OR.optByAvg     = False
                , OR.predefinedOR = Nothing
                }
  in   (return [])
  :<|> (\angHash -> liftIO $ Right <$> orFitHandler cfg "arche-ang" angHash)
  :<|> (\_user -> return NoContent)


orFitHandler :: OR.Cfg -> Text -> HashEBSD -> IO (OREvaluation)
orFitHandler cfg bucket (HashEBSD angHash) = do
  lgr  <- Google.newLogger Google.Info stdout

  env  <- Google.newEnv <&>
        (Google.envLogger .~ lgr)
      . (Google.envScopes .~ Storage.storageReadWriteScope)

  runResourceT . Google.runGoogle env $ do
    stream <- Google.download (Storage.objectsGet bucket angHash)
    ang    <- liftResourceT (runConduit (stream .| Conduit.sinkLbs))

    -- Force strictness on OR calculation otherwise the data
    --upload bellow can timeout while awaiting for calculation.
    (!orEval, vtk) <- liftIO $ OR.processEBSD cfg ang

    let
      body = Google.GBody ("application" // "octet-stream") (RequestBodyLBS $ renderUniVTK True vtk)
      vox_key = angHash <> ".vtk"
    
    void $ Google.upload (Storage.objectsInsert bucket Storage.object' & Storage.oiName ?~ vox_key) body

    return orEval

