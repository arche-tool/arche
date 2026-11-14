{-# LANGUAGE ScopedTypeVariables #-}

module Benchmark.OR
  ( benchOR
  ) where

import Criterion
import Criterion.Main
import System.Random (mkStdGen, random)
import qualified Data.Vector.Unboxed as U

import Arche.OR (OR(..), misoDoubleOR, genTS)
import Texture.Orientation ((#<=), (-#-), getAbsShortOmega, Quaternion(..))
import Texture.Symmetry (Symm(..), getSymmOps, getInFZ, SymmOp(..))

benchOR :: Benchmark
benchOR = 
  bgroup "misoDoubleOR" [ 
      bench "original" $ benchMiso misoDoubleOREta
    , bench "normal" $ benchMisoSO misoDoubleOR
    ]

benchMiso :: (U.Vector OR -> Symm -> Quaternion -> Quaternion -> Double) -> Benchmarkable
benchMiso probe = let
  s0 = mkStdGen 666
  (q1, s1) = random s0
  (q2, s2) = random s1
  (q3, _) = random s2
  ors = genTS Cubic . OR $ q3
  in nf (\(ors', qa, qb) -> probe ors' Cubic qa qb) (ors, q1, q2)

benchMisoSO :: (U.Vector OR -> U.Vector SymmOp -> Quaternion -> Quaternion -> Double) -> Benchmarkable
benchMisoSO probe = let
  s0 = mkStdGen 666
  (q1, s1) = random s0
  (q2, s2) = random s1
  (q3, _) = random s2
  ors = genTS Cubic . OR $ q3
  in nf (\(ors', qa, qb, ops) -> probe ors' ops qa qb) (ors, q1, q2, getSymmOps Cubic)

-- ======================= With eta =========================
misoDoubleOREta ::U.Vector OR -> Symm -> Quaternion -> Quaternion -> Double
misoDoubleOREta ors symm q1 q2 = let
  ks1 = U.map ((q1 #<=) . qOR) ors
  ks2 = U.map ((q2 #<=) . qOR) ors
  -- Fully correct. Need prove that works!
  foo q = U.map (getMisoAngleEta symm q) ks2
  in U.minimum $ U.concatMap foo ks1

getMisoAngleEta :: Symm -> Quaternion -> Quaternion -> Double
getMisoAngleEta symm = let
  foo = getAbsShortOmega . getInFZ (getSymmOps symm)
  -- avoiding eta expansion of q1 and q2 to memorize
  in \q1 q2 -> foo (q2 -#- q1)