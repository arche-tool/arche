module Main where

import Criterion.Main
import Criterion.Types (timeLimit)

import Benchmark.OR
import Benchmark.ORFit

main :: IO ()
main = do
  defaultMainWith defaultConfig{timeLimit = 2.0} [
      Benchmark.OR.benchOR
    , Benchmark.ORFit.benchORFit
    ]