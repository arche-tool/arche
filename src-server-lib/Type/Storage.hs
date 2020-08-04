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
    , StorageLink(..)
    , StorageObjectName(..)
    , imageBucket
    , ebsdBucket
    , landingZoneBucket
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

imageBucket :: StorageBucket
imageBucket  = StorageBucket "arche-image"

ebsdBucket :: StorageBucket
ebsdBucket = StorageBucket "ebsd"

landingZoneBucket :: StorageBucket
landingZoneBucket = StorageBucket "arche-landing-zone"

newtype StorageObjectName = StorageObjectName {objName :: Text} deriving (Show, Generic, Eq)

data StorageLink
    = StorageLink
    { objectName :: Text
    , signedLink :: Text
    } deriving (Generic, Show)

instance ToJSON StorageLink
instance FromJSON StorageLink

-- ============================
-- ======== Instances =========
-- ============================

-- ========= Document =========
instance ToDocValue HashEBSD
instance ToDocValue HashOR
instance ToDocValue HashArche
instance ToDocValue HashResult
instance ToDocValue StorageBucket
instance ToDocValue StorageLink

instance FromDocValue HashEBSD
instance FromDocValue HashOR
instance FromDocValue HashArche
instance FromDocValue HashResult
instance FromDocValue StorageBucket
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

instance FromHttpApiData StorageObjectName where
    parseUrlPiece txt = Right $ StorageObjectName txt


-- ========= ToHttp =========
instance ToHttpApiData HashEBSD where
    toUrlPiece (HashEBSD txt) = txt

instance ToHttpApiData HashOR where
    toUrlPiece (HashOR txt) = txt

instance ToHttpApiData HashArche where
    toUrlPiece (HashArche txt) = txt

instance ToHttpApiData HashResult where
    toUrlPiece (HashResult txt) = txt

instance ToHttpApiData StorageObjectName where
    toUrlPiece (StorageObjectName txt) = txt

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

instance ToJSON StorageObjectName
instance FromJSON StorageObjectName