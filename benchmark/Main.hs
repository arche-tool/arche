{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Criterion
import Criterion.Main
import Data.Map.Strict (fromList)
import System.Random (mkStdGen, random, randoms)
import qualified Data.List as L
import qualified Data.Vector.Unboxed as U

import Arche.OR (OR(..), misoDoubleOR, misoDoubleOR', genTS)
import Texture.Orientation ((#<=), (-#-), getAbsShortOmega, Quaternion(..))
import Texture.Symmetry (Symm(..), getSymmOps, getInFZ, SymmOp(..))

main :: IO ()
main = defaultMain [
  bgroup "reference" [ 
      bench "fib" $ benchFib 35
    , bench "malloc" $ benchMalloc
  ],
  
  bgroup "misoDoubleOR" [ 
      bench "with-eta" $ benchMiso misoDoubleOREta
    , bench "no-eta" $ benchMiso misoDoubleORNoEta
    , bench "normal" $ benchMiso misoDoubleOR
    , bench "improved" $ benchMiso misoDoubleOR'
    , bench "improved2" $ benchMiso2 misoDoubleOR2
    , bench "improved3" $ benchMiso3 misoDoubleOR3
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

benchMiso2 :: ([OR] -> Symm -> Quaternion -> Quaternion -> Double) -> Benchmarkable
benchMiso2 probe = let
  s0 = mkStdGen 666
  (q1, s1) = random s0
  (q2, s2) = random s1
  (q3, _) = random s2
  ors = U.toList . genTS . OR $ q3
  in nf (\(ors', qa, qb) -> probe ors' Cubic qa qb) (ors, q1, q2)

benchMiso3 :: (U.Vector OR -> U.Vector SymmOp -> Quaternion -> Quaternion -> Double) -> Benchmarkable
benchMiso3 probe = let
  s0 = mkStdGen 666
  (q1, s1) = random s0
  (q2, s2) = random s1
  (q3, _) = random s2
  ors = genTS . OR $ q3
  in nf (\(ors', qa, qb, ops) -> probe ors' ops qa qb) (ors, q1, q2, getSymmOps Cubic)

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

misoDoubleOR2 :: [OR] -> Symm -> Quaternion -> Quaternion -> Double
misoDoubleOR2 ors symm q1 q2 = let
  --symOps = getSymmOps symm
  -- Fully correct. Need prove that works!
  func :: OR -> Double
  func o_r = let
    ks1 = ((q1 #<=) . qOR) o_r
    ks2 = ((q2 #<=) . qOR) o_r
    theta = getMisoAngleEta symm ks1 ks2
    in theta
  in L.minimum $ map func ors

misoDoubleOR3 ::U.Vector OR -> U.Vector SymmOp -> Quaternion -> Quaternion -> Double
misoDoubleOR3 ors ops q1 q2 = let
  --symOps = getSymmOps symm
  -- Fully correct. Need prove that works!
  func :: OR -> Double
  func o_r = let
    ks1 = ((q1 #<=) . qOR) o_r
    ks2 = ((q2 #<=) . qOR) o_r
    theta = getMisoAngleNoEta ops ks1 ks2
    in theta
  in U.minimum $ U.map func ors


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


-- ======================= Reference Benchmark =========================
benchMalloc :: Benchmarkable
benchMalloc = let
  xs = zip (randoms (mkStdGen 0) :: [Int]) [1 :: Int .. 10000]
  in nf Data.Map.Strict.fromList xs

benchFib :: Int -> Benchmarkable
benchFib = nf fib
  where
    fib :: Int -> Int
    fib m
      | m < 0     = error "negative!"
      | otherwise = go m
      where
        go 0 = 0
        go 1 = 1
        go n = go (n-1) + go (n-2)