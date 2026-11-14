module Benchmark.ORFit
  ( benchORFit
  ) where

import Criterion
import qualified Data.ByteString.Lazy as BSL

import Arche.OR (avgError)
import Arche.Strategy.ORFitAll (OREvaluation(misfitError))
import Data.VTK (renderUniVTK)
import Texture.Orientation (Deg(..))
import qualified Arche.OR as OR
import qualified Arche.Strategy.ORFitAll as OR

benchORFit :: Benchmark
benchORFit = 
  bgroup "ORFit" [ 
      bench "full run" $ runORFit
    ]

runORFit :: Benchmarkable
runORFit = let
  cfg = OR.Cfg {
      OR.misoAngle    = Deg 5
    , OR.optByAvg     = False
    , OR.predefinedOR = Nothing
    , OR.startOR      = Nothing
    , OR.parentPhase  = Right OR.CubicPhase
    , OR.productPhase = OR.Phase 1 OR.CubicPhase
    }
  
  eval ang = do
    (orEval, vtk) <- OR.processEBSD cfg ang
    return (unDeg . avgError . misfitError $ orEval, renderUniVTK True vtk)

  in perRunEnv (BSL.readFile "benchmark/data/TestSample.ang") eval