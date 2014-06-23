{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}

module Gamma.Strategy.ORFitSingle
       ( run
       , Cfg(..)
       ) where

import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as U

import           System.FilePath

import           Hammer.VTK
import           Hammer.MicroGraph
import           Hammer.Math.Algebra

import           File.ANGReader
import           Texture.Orientation
import           Texture.Symmetry

import           Gamma.OMRender
import           Gamma.GBRender
import           Gamma.Grains
import           Gamma.OR

data Cfg =
  Cfg
  { misoAngle   :: Deg
  , ang_input   :: FilePath
  , base_output :: FilePath
  } deriving (Show)

run :: Cfg -> IO ()
run Cfg{..} = do
  ang <- parseANG ang_input
  let vboxQ = ebsdToVoxBox ang rotation
  case getGrainID misoAngle Cubic vboxQ of
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
        writeUniVTKfile (base_output <.> "SO3" <.> "vtu") True vtkSO3
        let
          (g, t)   = getGammaOR2 ang
          --g        = getGamma ang
          ggid     = U.singleton $ mkGrainID (-1)
          as       = U.map (toFZ Cubic . (g #<=) . qOR) (V.convert ksORs)
          agid     = U.replicate (U.length as) (mkGrainID $ -1)
          vtkSO3_g = renderSO3Points Cubic ND ggid (U.singleton g)
          vtkSO3_a = renderSO3Points Cubic ND agid as
        print (g, t)
        writeUniVTKfile (base_output <.> "SO3-gamma" <.> "vtu") True vtkSO3_g
        writeUniVTKfile (base_output <.> "SO3-alpha" <.> "vtu") True vtkSO3_a

getGammaOR2 :: EBSDdata -> (Quaternion, OR)
getGammaOR2 EBSDdata{..} = (gf, tf)
  where
    is = U.convert $ V.findIndices ((> 0.1) . ci) nodes
    qs = U.map (rotation . (nodes V.!)) is
    ef = uniformerrorfunc (U.map getQinFZ qs)
    g0 = hotStartGamma ef
    (gf, tf) = findGammaOR ef g0 ksOR

getGammaOR :: Int -> EBSDdata -> (Quaternion, OR)
getGammaOR n EBSDdata{..} = go n t0
  where
    t0 = mkOR (Vec3 1 1 2) (Deg 90)
    is = V.convert $ V.findIndices ((> 0.1) . ci) nodes
    qs = U.map (rotation . (nodes V.!)) is
    ef = uniformerrorfunc (U.map getQinFZ qs)
    g0 = hotStartGamma ef
    func t = let
      g = findGamma ef g0 (genTS t)
      in findOR ef g t
    go k t
      | k <= 0    = (findGamma ef g0 (genTS t), convert t)
      | otherwise = go (k-1) (func t)

getGamma :: EBSDdata -> Quaternion
getGamma EBSDdata{..} = let
  is = V.convert $ V.findIndices ((> 0.1) . ci) nodes
  qs = U.map (rotation . (nodes V.!)) is
  ef = uniformerrorfunc (U.map getQinFZ qs)
  g0 = hotStartGamma ef
  gamma = toFZ Cubic $ findGamma ef g0 ksORs
  in gamma
