module Api.Result where

import Data.Vector.Unboxed (Vector)
import Data.HashMap.Strict   (HashMap)
import Hammer.VoxBox
import Hammer.VoxBox.Render
import Linear.Vect (Vec3(..))

import qualified Data.Vector.Unboxed as V

data MicroStructure
    = MicroStructure
    { xVertices :: Vector Double
    , yVertices :: Vector Double
    , zVertices :: Vector Double
    , voxels :: Vector (((Int, Int), (Int, Int)), ((Int, Int), (Int, Int)))
    } deriving ()

type VoxelInfo a = HashMap Int a 

packVoxBox :: VoxBox a -> MicroStructure
packVoxBox vbox = let
  vbext = getExtendedVoxBox vbox
  ps    = getVoxBoxCornersPoints vbext
  in MicroStructure {
    xVertices = V.map (\(Vec3 x _ _) -> x) ps,
    yVertices = V.map (\(Vec3 x _ _) -> x) ps,
    zVertices = V.map (\(Vec3 x _ _) -> x) ps,
    voxels = V.empty
}