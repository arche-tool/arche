{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Criterion
import Criterion.Main
import System.Random (mkStdGen, random)
import qualified Data.Vector.Unboxed as U

import Arche.OR (OR(..), misoDoubleOR, misoDoubleOR', genTS)
import Texture.Orientation ((#<=), (-#-), getAbsShortOmega, Quaternion(..))
import Texture.Symmetry (Symm(..), getSymmOps, getInFZ, SymmOp(..))

main :: IO ()
main = defaultMain [
  bgroup "misoDoubleOR" [ 
      bench "with-eta" $ benchMiso misoDoubleOREta
    , bench "no-eta" $ benchMiso misoDoubleORNoEta
    ]
  ]

benchMiso :: (U.Vector OR -> Symm -> Quaternion -> Quaternion -> Double) -> Benchmarkable
benchMiso probe = let
  s0 = mkStdGen 666
  (q1, s1) = random s0
  (q2, s2) = random s1
  (q3, _) = random s2
  ors = genTS . OR $ q3
  in nf (\(ors', qa, qb) -> probe ors' Cubic qa qb) (ors, q1, q2)


-- ======================= No eta =========================
misoDoubleORNoEta ::U.Vector OR -> Symm -> Quaternion -> Quaternion -> Double
misoDoubleORNoEta ors symm q1 q2 = let
  ks1 = U.map ((q1 #<=) . qOR) ors
  ks2 = U.map ((q2 #<=) . qOR) ors
  -- Fully correct. Need prove that works!
  ops = getSymmOps symm
  foo q = U.map (getMisoAngleNoEta ops q) ks2
  in U.minimum $ U.concatMap foo ks1

getMisoAngleNoEta :: U.Vector SymmOp -> Quaternion -> Quaternion -> Double
getMisoAngleNoEta ops q1 q2 = getAbsShortOmega $ getInFZ ops (q2 -#- q1)


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