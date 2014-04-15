{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}

module Gamma.Strategy.Miyamoto
       ( run ) where

import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

import           Control.Parallel.Strategies

import           System.FilePath

import           Texture.Orientation
import           Hammer.VoxBox
import           Hammer.VTK
import           File.ANGReader              (parseANG, rotation, nodes)
import           Texture.Symmetry            (Symm (..))

import           Gamma.OMRender
import           Gamma.Grains
import           Gamma.OR

-- | Simlpe reconstruction strategy were the orientation map is divided in non-overlapping
-- areas and the parent phase is calculated from all product orientation within the subarea.
-- The calculation is done by function minimization with pure KS.
run :: FilePath -> FilePath -> IO ()
run fin fout = do
  ang <- parseANG fin
  let
    vb     = getVoxBox ang
    vb'    = scanBox 100 vb
    node'  = V.zipWith (\p q -> p {rotation = q}) (nodes ang) (U.convert $ grainID vb')
    ang'   = ang {nodes = node'}
    viewOM = [ showOMQI
             , showOMCI
             , showOMPhase
             , showOMIPF    Cubic ND
             ]
    vtkOM  = renderOM viewOM ang'
  writeUniVTKfile (fout <.> "vti") True vtkOM

scanBox :: Int -> VoxBox Quaternion -> VoxBox Quaternion
scanBox n vb@VoxBox{..} = vb {grainID = newVec}
  where
    stg = parListChunk 5000 (evalTuple2 rpar r0)
    l = U.length grainID
    newVec = U.create $ do
      v <- UM.new l
      let qis  = divConq dimension
          qisp = qis `using` stg
      mapM_ (save v) qisp
      return v
    save v (ga, is) = U.mapM_ (\i -> UM.write v i ga) is
    getG br = let
      ps = V.fromList $ getRangePos br
      is = V.convert $ V.map (dimension %@) ps
      qs = U.map (grainID U.!) is
      in (findGamma ksORs qs, is)
    divConq br
      | sizeVoxBoxRange br <= 0 = []
      | sizeVoxBoxRange br <= n = [getG br]
      | otherwise = case splitInTwoBox br of
        Just (br1, br2) -> divConq br1 ++ divConq br2
        Nothing         -> []
