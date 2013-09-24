{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

module Gamma.Grains
       ( getGrainID
       , getVoxBox
       ) where

import qualified Data.Vector as V

import           Control.Applicative ((<$>))
import           Hammer.MicroGraph   (GrainID)

import           Hammer.Reader.ANGReader
import           Hammer.Texture.Orientation
import           Hammer.Texture.Symmetry
import           Hammer.VoxBox.Base
import           Hammer.VoxBox.VoxConnFinder

getGrainID :: Symm -> EBSDdata -> Maybe (VoxBox GrainID)
getGrainID symm ed = let
  isGrain a b = let
    omega = getMisoAngle symm a b
    in (fromAngle $ Deg 15) > omega
  vbox     = getVoxBox ed
  vboxSymm = vbox { grainID = V.map (toFZ symm) (grainID vbox) }
  in fst . resetGrainIDs <$> grainFinder isGrain vboxSymm

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
  in VoxBox dime org spc (V.map rotation nodes)
