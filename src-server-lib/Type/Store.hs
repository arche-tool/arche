{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Type.Store where

import Data.Aeson    (ToJSON, FromJSON)
import Data.Hashable (Hashable)
import Data.Text     (Text)
import GHC.Generics
import GHC.Word      (Word8)

import qualified Arche.Strategy.ORFitAll   as OF
import qualified Arche.OR                  as OR
import qualified Texture.Orientation       as TO

import Type.Storage (
    HashEBSD(..),
    HashOR(..),
    HashArche(..), 
    HashResult(..), 
    StorageLink(..)
    )
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
instance FromJSON EBSD

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
instance FromJSON OR

instance ToDocValue OR
instance FromDocValue OR

-- ================ Arche ================
data Arche r
    = Arche
    { hashArche :: HashArche
    , cfgArche  :: ArcheCfg
    , results   :: [ArcheResult r]
    } deriving (Show, Generic)

data ArcheResult r
    = ArcheResult
    { mclFactor :: Double
    , parentIPF :: r
    , errorMap  :: r
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

instance Functor Arche where
    fmap func Arche{..}
        = Arche
        { hashArche = hashArche 
        , cfgArche  = cfgArche  
        , results   = map (fmap func) results   
        }

instance Functor ArcheResult where
    fmap func ArcheResult{..}
        = ArcheResult
        { mclFactor = mclFactor
        , parentIPF = func parentIPF
        , errorMap  = func errorMap
        }

instance Hashable ArcheCfg

instance ToJSON a => ToJSON (Arche a)
instance ToJSON a => ToJSON (ArcheResult a)
instance ToJSON ArcheCfg

instance FromJSON a => FromJSON (Arche a)
instance FromJSON a => FromJSON (ArcheResult a)
instance FromJSON ArcheCfg

instance ToDocValue a => ToDocValue (Arche a)
instance ToDocValue a => ToDocValue (ArcheResult a)
instance FromDocValue a => FromDocValue (Arche a)
instance FromDocValue a => FromDocValue (ArcheResult a)
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
instance FromJSON User

instance ToDocValue User
instance FromDocValue User