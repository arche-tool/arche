{-# LANGUAGE DeriveGeneric #-}

module Type.Result
  ( MicroStructure(..)
  , VoxelInfo
  , packVoxBox
  ) where

import Data.Aeson
import Data.HashMap.Strict   (HashMap)
import Data.Vector.Unboxed (Vector)
import GHC.Generics

import qualified Data.Vector.Unboxed as V

import Hammer.VoxBox
import Hammer.VoxBox.Render
import Linear.Vect (Vec3(..))

data MicroStructure
    = MicroStructure
    { xVertices :: Vector Double
    , yVertices :: Vector Double
    , zVertices :: Vector Double
    , voxels :: Vector (((Int, Int), (Int, Int)), ((Int, Int), (Int, Int)))
    } deriving (Generic, Show)

instance ToJSON MicroStructure

type VoxelInfo a = HashMap Int a 

packVoxBox :: VoxBox a -> MicroStructure
packVoxBox vbox = let
  vbext = getExtendedVoxBox vbox
  ps    = getVoxBoxCornersPoints vbext
  in MicroStructure {
    xVertices = V.map (\(Vec3 x _ _) -> x) ps,
    yVertices = V.map (\(Vec3 _ y _) -> y) ps,
    zVertices = V.map (\(Vec3 _ _ z) -> z) ps,
    voxels = V.fromList . map (renderVoxelVolume vbext) . getRangePos . dimension $ vbox 
    }