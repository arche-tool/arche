module Main where

import Criterion
import Criterion.Main
import System.Random (mkStdGen, random, randoms)
import Data.Map.Strict (fromList)

import Arche.OR (OR(..), misoDoubleOR, genTS)
import Texture.Symmetry (Symm(..))

main :: IO ()
main = defaultMain [
  bgroup "fib" [
      bench "25" $ whnf fib 25
    , bench "40" $ whnf fib 40
    ],
  
  bgroup "malloc" [ 
      bench "Data.Map.Strict.fromList" $ benchMalloc 
  ],
  
  bgroup "arche" [ 
      bench "misoDoubleOR-seed3" $ benchMisoDOR 3
    , bench "misoDoubleOR-seed7" $ benchMisoDOR 7
  ]
  ]

benchMisoDOR :: Int -> Benchmarkable
benchMisoDOR i = let
  s0 = mkStdGen i
  (q1, s1) = random s0
  (q2, s2) = random s1
  (q3, _) = random s2
  ors = genTS . OR $ q3
  in nf (\(ors', qa, qb) -> misoDoubleOR ors' Cubic qa qb) (ors, q1, q2)

benchMalloc :: Benchmarkable
benchMalloc = let
  xs = zip (randoms (mkStdGen 0) :: [Int]) [1 :: Int .. 1000000]
  in nf Data.Map.Strict.fromList xs

fib :: Int -> Int
fib m
  | m < 0     = error "negative!"
  | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n-1) + go (n-2)