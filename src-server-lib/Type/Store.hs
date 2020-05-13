{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric #-}

module Type.Store where

import Data.Aeson   (ToJSON)
import Data.Text    (Text)
import GHC.Generics

import qualified Arche.Strategy.GomesGraph as GG
import qualified Arche.Strategy.ORFitAll   as OF

import Type.Storage (HashEBSD(..), HashOR(..), HashArche(..))
import Util.FireStore
import Util.OrphanInstances ()

-- ================ EBSD ================
data EBSD
    = EBSD
    { alias     :: Text
    , hashEBSD  :: HashEBSD
    , createdBy :: User
    } deriving (Show, Generic)

instance ToJSON EBSD

instance ToDocValue EBSD
instance FromDocValue EBSD

-- ================ OR ================
data OR
    = OR
    { hashOR   :: HashOR
    , cfgOR    :: OF.Cfg
    , resultOR :: OF.OREvaluation
    } deriving (Show, Generic)

instance ToJSON OR

instance ToDocValue OR
instance FromDocValue OR

-- ================ Arche ================
data Arche
    = Arche
    { hashArche :: HashArche
    , cfgArche  :: GG.Cfg
    } deriving (Show, Generic)

instance ToJSON Arche

-- ================ User ================
data User
    = User
    { email :: Text
    , name :: Maybe Text
    } deriving (Show, Generic)

instance Eq User where
    (==) (User e1 _) (User e2 _) = e1 == e2

instance ToJSON User

instance ToDocValue User
instance FromDocValue User