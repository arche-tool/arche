module Arche.Grains
  ( getGrainID
  , getGrainID'
  , getGrainPhase
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Vector         (Vector)
import Hammer.MicroGraph   (GrainID)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as U

import Hammer.VoxBox
import Hammer.VoxConn
import Texture.Orientation
import Texture.Symmetry

import Arche.OR

getGrainID :: Deg -> Symm -> VoxBox Quaternion
           -> Maybe (VoxBox GrainID, HashMap Int (Vector VoxelPos))
getGrainID mis symm vbox = let
  isGrain qa qb = let
    omega = getMisoAngle symm qa qb
    in abs (fromAngle mis) > omega
  vboxSymm = vbox { grainID = U.map (toFZ symm) (grainID vbox) }
  in resetGrainIDs <$> grainFinder isGrain vboxSymm

getGrainID' :: Deg -> Symm -> VoxBox (Quaternion, PhaseID)
            -> Maybe (VoxBox GrainID, HashMap Int (Vector VoxelPos))
getGrainID' mis symm vbox = let
  isGrain (qa, pa) (qb, pb) = let
    omega = getMisoAngle symm qa qb
    in pa == pb && abs (fromAngle mis) > omega
  in resetGrainIDs <$> grainFinder isGrain vbox

getGrainPhase :: VoxBox (Quaternion, PhaseID) -> HashMap Int (V.Vector VoxelPos)
              -> Int -> Maybe PhaseID
getGrainPhase vbqp gmap gid = HM.lookup gid gmap >>= func
  where func vs
          | V.null vs = Nothing
          | otherwise = return $ snd $ vbqp #! V.head vs
