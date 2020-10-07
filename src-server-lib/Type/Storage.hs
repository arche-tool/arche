{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Type.Storage
    ( HashEBSD(..)
    , HashOR(..)
    , HashArche(..)
    , HashResult(..)
    , StorageBucket(bktName)
    , TaskQueue(queueName)
    , StorageLink(..)
    , StoragePublicLink(..)
    , StorageObject(..)
    , StorageObjectName(..)
    , imageBucket
    , ebsdBucket
    , landingZoneBucket
    , reconstructionQueue
    , orFitQueue
    ) where

import GHC.Generics
import Data.Aeson    (ToJSON, FromJSON)
import Data.Text     (Text)
import Servant       (FromHttpApiData(..), ToHttpApiData(..))

import Util.FireStore

newtype HashEBSD   = HashEBSD   Text deriving (Show, Generic, Eq)
newtype HashOR     = HashOR     Text deriving (Show, Generic, Eq)
newtype HashArche  = HashArche  Text deriving (Show, Generic, Eq)
newtype HashResult = HashResult Text deriving (Show, Generic, Eq)

newtype StorageBucket = StorageBucket {bktName :: Text} deriving (Show, Generic, Eq)

newtype StorageObjectName = StorageObjectName {objFullName :: Text} deriving (Show, Generic, Eq)

newtype TaskQueue = TaskQueue {queueName :: Text} deriving (Show, Generic, Eq)

imageBucket :: StorageBucket
imageBucket  = StorageBucket "arche-image"

ebsdBucket :: StorageBucket
ebsdBucket = StorageBucket "ebsd"

landingZoneBucket :: StorageBucket
landingZoneBucket = StorageBucket "arche-landing-zone"

reconstructionQueue :: TaskQueue
reconstructionQueue = TaskQueue "projects/apt-muse-269419/locations/europe-west1/queues/reconstruction-queue"

orFitQueue :: TaskQueue
orFitQueue = TaskQueue "projects/apt-muse-269419/locations/europe-west1/queues/orfit-queue"


data StorageObject
    = StorageObject
    { objName      :: Text
    , objExtension :: Maybe Text
    , objBucket    :: StorageBucket
    } deriving (Show, Generic, Eq)

data StorageLink
    = StorageLink
    { objectName :: Text
    , signedLink :: Text
    } deriving (Generic, Show)

data StoragePublicLink
    = StoragePublicLink
    { publicName :: Text
    , publicLink :: Text
    } deriving (Generic, Show)

-- ============================
-- ======== Instances =========
-- ============================

-- ========= Document =========
instance ToDocValue HashEBSD
instance ToDocValue HashOR
instance ToDocValue HashArche
instance ToDocValue HashResult
instance ToDocValue StorageBucket
instance ToDocValue StorageObject
instance ToDocValue StorageLink

instance FromDocValue HashEBSD
instance FromDocValue HashOR
instance FromDocValue HashArche
instance FromDocValue HashResult
instance FromDocValue StorageBucket
instance FromDocValue StorageObject
instance FromDocValue StorageLink

-- ========= FromHttp =========
instance FromHttpApiData HashEBSD where
    parseUrlPiece txt = Right $ HashEBSD txt

instance FromHttpApiData HashOR where
    parseUrlPiece txt = Right $ HashOR txt

instance FromHttpApiData HashArche where
    parseUrlPiece txt = Right $ HashArche txt

instance FromHttpApiData HashResult where
    parseUrlPiece txt = Right $ HashResult txt

-- ========= ToHttp =========
instance ToHttpApiData HashEBSD where
    toUrlPiece (HashEBSD txt) = txt

instance ToHttpApiData HashOR where
    toUrlPiece (HashOR txt) = txt

instance ToHttpApiData HashArche where
    toUrlPiece (HashArche txt) = txt

instance ToHttpApiData HashResult where
    toUrlPiece (HashResult txt) = txt

-- ========= JSON =========
instance ToJSON HashEBSD
instance FromJSON HashEBSD

instance ToJSON HashOR
instance FromJSON HashOR

instance ToJSON HashArche
instance FromJSON HashArche

instance ToJSON HashResult
instance FromJSON HashResult

instance ToJSON StorageBucket
instance FromJSON StorageBucket

instance ToJSON StorageObject
instance FromJSON StorageObject

instance ToJSON StorageObjectName
instance FromJSON StorageObjectName

instance ToJSON StorageLink
instance FromJSON StorageLink

instance ToJSON StoragePublicLink
instance FromJSON StoragePublicLink