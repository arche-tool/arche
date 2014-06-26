{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}

module Gamma.Strategy.Miyamoto
       ( run
       , Cfg(..)
       ) where

import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

import           Control.Parallel.Strategies
import           System.FilePath

import           Hammer.VoxBox
import           Hammer.VTK

import           File.ANGReader
import           Texture.Symmetry
import           Texture.Orientation

import           Gamma.OMRender
import           Gamma.OR

data Cfg =
  Cfg
  { boxSize     :: Int -- default = 100
  , ang_input   :: FilePath
  , base_output :: FilePath
  } deriving (Show)

-- | Simlpe reconstruction strategy were the orientation map is divided in non-overlapping
-- areas and the parent phase is calculated from all product orientation within the subarea.
-- The calculation is done by function minimization with pure KS.
run :: Cfg -> IO ()
run Cfg{..} = do
  ang <- parseANG ang_input
  vbq <- case ebsdToVoxBox ang rotation of
    Right x -> return x
    Left s  -> error s
  let
    vb'    = scanBox (max 1 boxSize) vbq
    node'  = V.zipWith (\p q -> p {rotation = q}) (nodes ang) (U.convert $ grainID vb')
    ang'   = ang {nodes = node'}
    viewOM = [ showOMQI
             , showOMCI
             , showOMPhase
             , showOMIPF    Cubic ND
             ]
    vtkOM  = renderOM viewOM ang'
  writeUniVTKfile (base_output <.> "vti") True vtkOM

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
      ef = uniformerrorfunc (U.map getQinFZ qs)
      g0 = hotStartGamma ef
      in (findGamma ef g0 ksORs, is)
    divConq br
      | sizeVoxBoxRange br <= 0 = []
      | sizeVoxBoxRange br <= n = [getG br]
      | otherwise = case splitInTwoBox br of
        Just (br1, br2) -> divConq br1 ++ divConq br2
        Nothing         -> []
