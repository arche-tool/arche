{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Type.API
    ( API
    , ORFitAPI
    , HashEBSD(..)
    , StorageBucket(bktText)
    , voxelBucket
    , facesBucket
    , edgesBucket
    , vertexBucket
    , angBucket
    , ctfBucket
    ) where

import Data.Aeson
import GHC.Generics
import Servant
import Data.Text (Text)

import Arche.Strategy.ORFitAll (OREvaluation)
import qualified Arche.Strategy.ORFitAll as OR

type API = ORFitAPI

type ORFitAPI = "orfit" :>
  (                               Get  '[JSON] [Either String OREvaluation]
  :<|> Capture "ang" HashEBSD  :> Get  '[JSON] (Either String OREvaluation)
  :<|> ReqBody '[JSON] OR.Cfg  :> Post '[JSON] NoContent
  )

newtype HashEBSD = HashEBSD Text deriving (Show, Generic, Eq)
instance FromJSON HashEBSD
instance ToJSON   HashEBSD
instance FromHttpApiData HashEBSD where
    parseUrlPiece txt = Right $ HashEBSD txt

newtype StorageBucket = StorageBucket {bktText :: Text} deriving (Show, Generic, Eq)
instance FromJSON StorageBucket
instance ToJSON   StorageBucket

voxelBucket :: StorageBucket
voxelBucket  = StorageBucket "voxel"

facesBucket :: StorageBucket
facesBucket  = StorageBucket "faces"

edgesBucket :: StorageBucket
edgesBucket  = StorageBucket "edges"

vertexBucket :: StorageBucket
vertexBucket = StorageBucket "vertex"

angBucket :: StorageBucket
angBucket = StorageBucket "ang"

ctfBucket :: StorageBucket
ctfBucket = StorageBucket "ctf"