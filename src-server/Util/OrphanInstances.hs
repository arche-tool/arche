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

instance Hashable TO.Quaternion

instance (Hashable a) => Hashable (LV.Vec2 a)
instance (Hashable a) => Hashable (LV.Vec3 a)
instance (Hashable a) => Hashable (LV.Vec4 a)

instance (Hashable a) => Hashable (Vector a) where
    hashWithSalt i = hashWithSalt i .toList 