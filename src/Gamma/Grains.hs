{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

module Gamma.Grains
       ( getGrainID
       ) where

import qualified Data.Vector.Unboxed as U

import           Control.Applicative ((<$>))
import           Hammer.MicroGraph   (GrainID)
import           Data.Vector         (Vector)
import           Data.HashMap.Strict (HashMap)

import           Texture.Orientation
import           Texture.Symmetry
import           Hammer.VoxBox
import           Hammer.VoxConn

getGrainID :: Deg -> Symm -> VoxBox Quaternion -> Maybe (VoxBox GrainID, HashMap Int (Vector VoxelPos))
getGrainID mis symm vbox = let
  isGrain a b = let
    omega = getMisoAngle symm a b
    in (abs $ fromAngle mis) > omega
  vboxSymm = vbox { grainID = U.map (toFZ symm) (grainID vbox) }
  in resetGrainIDs <$> grainFinder isGrain vboxSymm
