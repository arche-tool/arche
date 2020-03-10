{-# OPTIONS_GHC -fno-warn-orphans #-}

module Util.OrphanInstances where

import Data.Aeson (ToJSON, FromJSON)

import qualified Arche.OR                as OR
import qualified Arche.Strategy.ORFitAll as OF
import qualified Texture.Orientation     as TO
import qualified Linear.Vect             as LV

-- ========= to JSON =========
instance ToJSON OF.Cfg
instance ToJSON OF.OrientationRelationship
instance ToJSON OF.KSDeviation
instance ToJSON OF.OREvaluation

instance ToJSON OR.FitError
instance ToJSON OR.OR

instance ToJSON TO.Deg
instance ToJSON TO.Quaternion
instance ToJSON TO.AxisPair

instance (ToJSON a) => ToJSON (LV.Vec2 a)
instance (ToJSON a) => ToJSON (LV.Vec3 a)
instance (ToJSON a) => ToJSON (LV.Vec4 a)

-- ========= from JSON =========
instance FromJSON OF.Cfg

instance FromJSON TO.AxisPair
instance FromJSON TO.Deg

instance (FromJSON a) => FromJSON (LV.Vec2 a)
instance (FromJSON a) => FromJSON (LV.Vec3 a)
instance (FromJSON a) => FromJSON (LV.Vec4 a)
