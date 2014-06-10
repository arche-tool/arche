{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}

module Gamma.Strategy.ORFitSingle
       ( run ) where

import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as U

import           System.FilePath

import           Hammer.VTK
import           Hammer.MicroGraph

import           File.ANGReader
import           Texture.Orientation
import           Texture.Symmetry

import           Gamma.OMRender
import           Gamma.GBRender
import           Gamma.Grains
import           Gamma.OR

run :: Deg -> FilePath -> FilePath -> IO ()
run miso fin fout = do
  ang <- parseANG fin
  let vboxQ = ebsdToVoxBox ang rotation
  case getGrainID miso Cubic vboxQ of
    Nothing        -> print "No grain detected!"
    Just (gids, _) -> let
      viewGB = [ showGBMiso   Cubic
               , showGBMisoKS Cubic
               ]
      viewOM = [ showOMQI
               , showOMCI
               , showOMPhase
               , showOMIPF    Cubic ND
               , showGrainIDs gids
               ]
      (vecQ, vecGID) = getOriGID ang gids
      vtkGB  = renderGB viewGB vboxQ
      vtkOM  = renderOM viewOM ang
      vtkSO3 = renderSO3Points Cubic ND vecGID vecQ
      in do
        print (U.length vecQ, U.length vecGID)
        --writeUniVTKfile (fout <.> "vti") True vtkOM
        --writeUniVTKfile (fout <.> "vtu") True vtkGB
        writeUniVTKfile (fout <.> "SO3" <.> "vtu") True vtkSO3
        let
          (g, t)   = getGammaOR2 ang
          --g        = getGamma ang
          ggid     = U.singleton $ mkGrainID (-1)
          as       = U.map (toFZ Cubic . (g #<=) . qOR) (V.convert ksORs)
          agid     = U.replicate (U.length as) (mkGrainID $ -1)
          vtkSO3_g = renderSO3Points Cubic ND              ggid (U.singleton g)
          vtkSO3_a = renderSO3Points Cubic ND              agid as
        print (g, t)
        writeUniVTKfile (fout <.> "SO3-gamma" <.> "vtu") True vtkSO3_g
        writeUniVTKfile (fout <.> "SO3-alpha" <.> "vtu") True vtkSO3_a
