{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

module Gamma.Grains
       ( getGrainID
       , getVoxBox
       ) where

import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as U

import           Control.Applicative ((<$>))
import           Hammer.MicroGraph   (GrainID)
import           Data.Vector         (Vector)
import           Data.HashMap.Strict (HashMap)

import           File.ANGReader
import           Texture.Orientation
import           Texture.Symmetry
import           Hammer.VoxBox.Base
import           Hammer.VoxBox.VoxConnFinder

getGrainID :: Deg -> Symm -> EBSDdata -> Maybe (VoxBox GrainID, HashMap Int (Vector VoxelPos))
getGrainID mis symm ed = let
  isGrain a b = let
    omega = getMisoAngle symm a b
    in (abs $ fromAngle mis) > omega
  vbox     = getVoxBox ed
  vboxSymm = vbox { grainID = U.map (toFZ symm) (grainID vbox) }
  in resetGrainIDs <$> grainFinder isGrain vboxSymm

getVoxBox :: EBSDdata -> VoxBox Quaternion
getVoxBox EBSDdata{..} = let
  EBSDinfo{..}       = ebsdInfo
  Gridinfo{..}       = grid
  (xstep, ystep)     = xystep
  (row, col_even, _) = rowCols
  boxorg = VoxelPos 1 1 1
  boxdim = VoxBoxDim col_even row 1
  dime   = VoxBoxRange boxorg boxdim
  org    = VoxBoxOrigin xstart ystart zstart
  spc    = VoxelDim xstep ystep ((xstep + ystep) / 2)
  in VoxBox dime org spc (V.convert $ V.map rotation nodes)
