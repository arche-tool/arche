{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards      #-}

module Util.OrphanInstances where

import Data.Vector (Vector, toList)

import Data.Aeson (ToJSON, FromJSON)
import Data.Hashable (Hashable, hashWithSalt)
import Servant (FromHttpApiData(..))

import qualified Arche.OR                  as OR
import qualified Arche.Strategy.GomesGraph as GG
import qualified Arche.Strategy.ORFitAll   as OF
import qualified File.EBSD                 as F
import qualified File.ANGReader            as F
import qualified File.CTFReader            as F
import qualified Texture.Orientation       as TO
import qualified Linear.Vect               as LV

import Util.FireStore.Value

-- ========= to JSON =========
instance ToJSON GG.Cfg

instance ToJSON OF.Cfg
instance ToJSON OF.OrientationRelationship
instance ToJSON OF.KSDeviation
instance ToJSON OF.OREvaluation

instance ToJSON OR.FitError
instance ToJSON OR.PhaseID
instance ToJSON OR.OR

instance ToJSON TO.Deg
instance ToJSON TO.Quaternion
instance ToJSON TO.AxisPair

instance (ToJSON a) => ToJSON (LV.Vec2 a)
instance (ToJSON a) => ToJSON (LV.Vec3 a)
instance (ToJSON a) => ToJSON (LV.Vec4 a)

-- ========= from JSON =========
instance FromJSON GG.Cfg

instance FromJSON OF.Cfg

instance FromJSON OR.PhaseID

instance FromJSON TO.AxisPair
instance FromJSON TO.Deg

instance (FromJSON a) => FromJSON (LV.Vec2 a)
instance (FromJSON a) => FromJSON (LV.Vec3 a)
instance (FromJSON a) => FromJSON (LV.Vec4 a)

-- ========= hashable =========
instance Hashable F.EBSDdata

instance Hashable F.ANGpoint
instance Hashable F.ANGgrid
instance Hashable F.ANGphase
instance Hashable F.ANGdata where
    hashWithSalt i F.ANGdata{..} = hashWithSalt i
        [ hashWithSalt i nodes
        , hashWithSalt i grid
        , hashWithSalt i phases
        ]

instance Hashable F.CTFpoint
instance Hashable F.CTFgrid
instance Hashable F.CTFphase
instance Hashable F.CTFdata where
    hashWithSalt i F.CTFdata{..} = hashWithSalt i
        [ hashWithSalt i nodes
        , hashWithSalt i grid
        , hashWithSalt i phases
        ]

instance Hashable TO.AxisPair
instance Hashable TO.Deg
instance Hashable TO.Quaternion

instance (Hashable a) => Hashable (LV.Vec2 a)
instance (Hashable a) => Hashable (LV.Vec3 a)
instance (Hashable a) => Hashable (LV.Vec4 a)

instance (Hashable a) => Hashable (Vector a) where
    hashWithSalt i = hashWithSalt i .toList 

instance Hashable OF.Cfg


-- ========= Document =========
instance ToDocValue OF.Cfg
instance ToDocValue OF.OREvaluation
instance ToDocValue OF.OrientationRelationship
instance ToDocValue OF.KSDeviation
instance ToDocValue TO.Deg
instance ToDocValue TO.AxisPair
instance ToDocValue TO.Quaternion
instance ToDocValue OR.FitError
instance ToDocValue OR.OR

instance (ToDocValue a) => ToDocValue (LV.Vec3 a)
instance (ToDocValue a) => ToDocValue (LV.Vec2 a)
instance (ToDocValue a) => ToDocValue (LV.Vec4 a)

instance FromDocValue OF.Cfg
instance FromDocValue OF.OREvaluation
instance FromDocValue OF.OrientationRelationship
instance FromDocValue OF.KSDeviation
instance FromDocValue TO.Deg
instance FromDocValue TO.AxisPair
instance FromDocValue TO.Quaternion
instance FromDocValue OR.FitError
instance FromDocValue OR.OR

instance (FromDocValue a) => FromDocValue (LV.Vec3 a)
instance (FromDocValue a) => FromDocValue (LV.Vec2 a)
instance (FromDocValue a) => FromDocValue (LV.Vec4 a)