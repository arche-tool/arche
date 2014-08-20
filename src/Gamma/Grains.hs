{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

module Gamma.Grains
       ( getGrainID
       , getGrainID'
       , getGrainPhase
       ) where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector         as V
import qualified Data.HashMap.Strict as HM

import           Control.Applicative ((<$>))
import           Hammer.MicroGraph   (GrainID)
import           Data.Vector         (Vector)
import           Data.HashMap.Strict (HashMap)

import           Texture.Orientation
import           Texture.Symmetry
import           Hammer.VoxBox
import           Hammer.VoxConn

getGrainID :: Deg -> Symm -> VoxBox Quaternion
           -> Maybe (VoxBox GrainID, HashMap Int (Vector VoxelPos))
getGrainID mis symm vbox = let
  isGrain qa qb = let
    omega = getMisoAngle symm qa qb
    in (abs $ fromAngle mis) > omega
  vboxSymm = vbox { grainID = U.map (toFZ symm) (grainID vbox) }
  in resetGrainIDs <$> grainFinder isGrain vboxSymm

getGrainID' :: Deg -> Symm -> VoxBox (Quaternion, Int)
            -> Maybe (VoxBox GrainID, HashMap Int (Vector VoxelPos))
getGrainID' mis symm vbox = let
  isGrain (qa, pa) (qb, pb) = let
    omega = getMisoAngle symm qa qb
    in pa == pb && (abs $ fromAngle mis) > omega
  --vboxSymm = vbox { grainID = U.map (toFZ symm) (grainID vbox) }
  in resetGrainIDs <$> grainFinder isGrain vbox

getGrainPhase :: VoxBox (Quaternion, Int) -> HashMap Int (V.Vector VoxelPos)
              -> Int -> Maybe Int
getGrainPhase vbqp gmap gid = HM.lookup gid gmap >>= func
  where func vs
          | V.null vs = Nothing
          | otherwise = return $ snd $ vbqp #! V.head vs
