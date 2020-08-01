{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeOperators       #-}

module Util.Client
  ( ArcheApiClient(..)
  , ORApiClient(..)
  , EBSDApiClient (..) 
  , ApiClient(..)
  , mkApiClient
  , getRequestOnBaseUrl'
  , getRequestOnBaseUrl
  ) where

import Control.Monad.Free
import Data.Text           (Text)
import GHC.Exception.Type  (displayException)
import Network.HTTP.Client (Request)
import Servant
import Servant.Client.Free

import qualified Servant.Client.Internal.HttpClient as I

import qualified Arche.Strategy.ORFitAll as OR

import Type.API
import Type.Storage
import Type.Store

-- ============================================
-- ==========   Arche Api Client   ============
-- ============================================

data ArcheApiClient m
  = ArcheApiClient 
  { getArches      :: m [Arche]
  , getArche       :: HashArche -> m Arche
  , postArche      :: ArcheCfg  -> m Arche
  , postArcheAsync :: ArcheCfg  -> m Text
  }

-- ============================================
-- ==========     OR Api Client    ============
-- ============================================

data ORApiClient m
  = ORApiClient 
  { getORs :: m [OR]
  , getOR  :: HashOR -> m OR
  , postOR :: OR.Cfg  -> m OR
  }

-- ============================================
-- ==========   Arche Api Client   ============
-- ============================================

data EBSDApiClient m
  = EBSDApiClient 
  { getEBSDs      :: m [EBSD]
  , getEBSD       :: HashEBSD -> m EBSD
  , getUploadLink :: m StorageLink
  , postEBSD      :: StorageObjectName -> m EBSD
  }

data ApiClient m
  = ApiClient
  { ebsdApiClient  :: EBSDApiClient m
  , orApiClient    :: HashEBSD -> ORApiClient m
  , archeApiClient :: HashEBSD -> HashOR -> ArcheApiClient m
  }

mkApiClient :: ApiClient (Free ClientF)
mkApiClient = ApiClient{..}
  where
    (ebsdEndpoints :<|> orEndpoints :<|> archeEndpoints) = client (Proxy :: Proxy API)

    ebsdApiClient :: EBSDApiClient (Free ClientF)
    ebsdApiClient = EBSDApiClient{..}
      where
        (postEBSD :<|> getEBSD :<|> getUploadLink :<|> getEBSDs) = ebsdEndpoints

    orApiClient :: HashEBSD -> ORApiClient (Free ClientF)
    orApiClient hashE = ORApiClient{..}
      where
        (getORs :<|> getOR :<|> postOR) = orEndpoints hashE
    
    archeApiClient :: HashEBSD -> HashOR -> ArcheApiClient (Free ClientF)
    archeApiClient hashE hashO = ArcheApiClient{..}
      where
        (getArches :<|> getArche :<|> postArche :<|> postArcheAsync) = archeEndpoints hashE hashO 

getRequestOnBaseUrl :: Show a => String -> Free ClientF a -> Either String Request
getRequestOnBaseUrl base cli = do
      burl <- either (Left . displayException) (Right) $ parseBaseUrl base
      getRequestOnBaseUrl' burl cli

getRequestOnBaseUrl' :: Show a => BaseUrl -> Free ClientF a -> Either String Request
getRequestOnBaseUrl' base cli = case cli of
    Pure n                  -> fail $ "ERROR: got pure result: " ++ show n
    Free (Throw err)        -> fail $ "ERROR: got error right away: " ++ show err
    Free (RunRequest req _) -> return $ I.requestToClientRequest base req
