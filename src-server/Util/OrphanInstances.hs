{-# OPTIONS_GHC -fno-warn-orphans #-}

module Util.OrphanInstances where

import Data.Aeson (ToJSON)

import qualified Arche.OR                as OR
import qualified Arche.Strategy.ORFitAll as OF
import qualified Texture.Orientation     as TO
import qualified Linear.Vect             as LV

instance ToJSON OF.OrientationRelationship
instance ToJSON OF.KSDeviation
instance ToJSON OF.OREvaluation
instance ToJSON OR.FitError
instance ToJSON OR.OR

instance ToJSON TO.Deg
instance ToJSON TO.Quaternion

instance (ToJSON a) => ToJSON (LV.Vec2 a)
instance (ToJSON a) => ToJSON (LV.Vec3 a)
instance (ToJSON a) => ToJSON (LV.Vec4 a)