{-# LANGUAGE BangPatterns #-}
module Arche.Grains
  ( getGrainID
  , getGrainPhase
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Vector         (Vector)
import Hammer.MicroGraph   (GrainID)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as V

import Hammer.VoxBox
import Hammer.VoxConn
import Texture.Orientation
import Texture.Symmetry

import Arche.OR

getGrainID
  :: Deg
  -> VoxBox (Quaternion, Phase)
  -> Maybe (VoxBox GrainID, HashMap Int (Vector VoxelPos))
getGrainID !mis vbox = let
  isGrain (!qa, !pa) (!qb, !pb)
    | pa == pb = let
        !omega = getMisoAngle (getPhaseSymm pa) qa qb
        in abs (fromAngle mis) > omega
    | otherwise = False
  in resetGrainIDs <$> grainFinder isGrain vbox

getGrainPhase
  :: VoxBox (Quaternion, Phase)
  -> HashMap Int (V.Vector VoxelPos)
  -> Int
  -> Maybe Phase
getGrainPhase vbqp gmap gid = HM.lookup gid gmap >>= func
  where func vs
          | V.null vs = Nothing
          | otherwise = return $ snd $ vbqp #! V.head vs
