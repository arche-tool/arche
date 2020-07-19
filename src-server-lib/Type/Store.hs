{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric #-}

module Type.Store where

import Data.Aeson    (ToJSON, FromJSON)
import Data.Hashable (Hashable)
import Data.Text     (Text)
import GHC.Generics
import GHC.Word      (Word8)

import qualified Arche.Strategy.ORFitAll   as OF
import qualified Arche.OR                  as OR
import qualified Texture.Orientation       as TO

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
    , cfgArche  :: ArcheCfg
    } deriving (Show, Generic)

data ArcheCfg = ArcheCfg
  { misoAngle              :: TO.Deg
  , excludeFloatingGrains  :: Bool
  , refinementSteps        :: Word8
  , initClusterFactor      :: Double
  , stepClusterFactor      :: Double
  , badAngle               :: TO.Deg
  , parentPhaseID          :: Maybe OR.PhaseID
  } deriving (Show, Generic)

instance Hashable ArcheCfg

instance ToJSON Arche
instance ToJSON ArcheCfg

instance FromJSON Arche
instance FromJSON ArcheCfg

instance ToDocValue Arche
instance FromDocValue Arche
instance ToDocValue ArcheCfg
instance FromDocValue ArcheCfg

-- ================ User ================
data User
    = User
    { id_number :: Text
    , email :: Maybe Text
    , name :: Maybe Text
    } deriving (Show, Generic)

instance Eq User where
    (==) e1 e2 = id_number e1 == id_number e2

instance ToJSON User

instance ToDocValue User
instance FromDocValue User