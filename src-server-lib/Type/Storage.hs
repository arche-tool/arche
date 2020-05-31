{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Type.Storage
    ( HashEBSD(..)
    , HashOR(..)
    , HashArche(..)
    , StorageBucket(bktText)
    , StorageLink(..)
    , voxelBucket
    , facesBucket
    , edgesBucket
    , vertexBucket
    , ebsdBucket
    , landingZoneBucket
    ) where

import Control.Lens ((&), (?~))
import GHC.Generics
import Data.Aeson    (ToJSON, FromJSON)
import Data.Hashable (Hashable, hashWithSalt)
import Data.Text     (Text)
import Servant       (FromHttpApiData(..))

import qualified Network.Google.FireStore as FireStore

import Util.FireStore

newtype HashEBSD  = HashEBSD  Text deriving (Show, Generic, Eq)
newtype HashOR    = HashOR    Text deriving (Show, Generic, Eq)
newtype HashArche = HashArche Text deriving (Show, Generic, Eq)

newtype StorageBucket = StorageBucket {bktText :: Text} deriving (Show, Generic, Eq)

voxelBucket :: StorageBucket
voxelBucket  = StorageBucket "voxel"

facesBucket :: StorageBucket
facesBucket  = StorageBucket "faces"

edgesBucket :: StorageBucket
edgesBucket  = StorageBucket "edges"

vertexBucket :: StorageBucket
vertexBucket = StorageBucket "vertex"

ebsdBucket :: StorageBucket
ebsdBucket = StorageBucket "ebsd"

landingZoneBucket :: StorageBucket
landingZoneBucket = StorageBucket "arche-landing-zone"

data StorageLink
    = StorageLink
    { objectPath :: Text
    , signedLink :: Text
    } deriving (Generic, Show)

instance ToJSON StorageLink

-- ============================
-- ======== Instances =========
-- ============================

-- ========= Document =========
instance ToDocValue HashEBSD
instance ToDocValue HashOR
instance ToDocValue HashArche
instance ToDocValue StorageBucket

instance FromDocValue HashEBSD
instance FromDocValue HashOR
instance FromDocValue HashArche
instance FromDocValue StorageBucket

-- ========= FromHttp =========
instance FromHttpApiData HashEBSD where
    parseUrlPiece txt = Right $ HashEBSD txt

instance FromHttpApiData HashOR where
    parseUrlPiece txt = Right $ HashOR txt

instance FromHttpApiData HashArche where
    parseUrlPiece txt = Right $ HashArche txt

-- ========= JSON =========
instance ToJSON HashEBSD
instance FromJSON HashEBSD

instance ToJSON HashOR
instance FromJSON HashOR

instance ToJSON HashArche
instance FromJSON HashArche

instance ToJSON StorageBucket
instance FromJSON StorageBucket