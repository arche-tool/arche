module Arche.TestOR
  ( test
  ) where

import Data.Vector.Unboxed (Vector)
import Test.QuickCheck
import Test.Tasty

import qualified Data.Vector.Unboxed   as U
import qualified Test.Tasty.QuickCheck as QC

import Arche.OR
import Texture.Orientation
import Texture.Symmetry

import Utils

test :: TestTree
test = testGroup "OR"
  [ QC.testProperty "misoSingleOR" testMisoSingleOR
  , QC.testProperty "misoDoubleOR" testMisoDoubleOR
  , QC.testProperty "misoDoubleORTransitive"   testMisoDoubleORTransitive
  ]

testMisoSingleOR :: OR -> Quaternion -> Quaternion -> Property
testMisoSingleOR ora q1 q2 = let
  ors = genTS Cubic ora
  symOps = getSymmOps Cubic
  in binaryTest "matchRef" (~=) (misoSingleOR ors symOps q1 q2) (misoSingleORRef ors Cubic q1 q2)

testMisoDoubleOR :: OR -> Quaternion -> Quaternion -> Property
testMisoDoubleOR ora q1 q2 = let
  ors = genTS Cubic ora
  symOps = getSymmOps Cubic
  in binaryTest "matchRef" (~=) (misoDoubleOR ors symOps q1 q2) (misoDoubleORRef ors Cubic q1 q2)

testMisoDoubleORTransitive :: OR -> Quaternion -> Quaternion -> Property
testMisoDoubleORTransitive  ora q1 q2 = let
  ors = genTS Cubic ora
  symOps = getSymmOps Cubic
  in binaryTest "nor" (~=) (misoDoubleOR    ors symOps q1 q2) (misoDoubleOR    ors symOps q2 q1)  .&&.
     binaryTest "ref" (~=) (misoDoubleORRef ors Cubic  q1 q2) (misoDoubleORRef ors Cubic  q2 q1)

misoDoubleORRef :: Vector OR -> Symm -> Quaternion -> Quaternion -> Double
misoDoubleORRef ors symm q1 q2 = let
  ks1 = U.map ((q1 #<=) . qOR) ors
  ks2 = U.map ((q2 #<=) . qOR) ors
  foo q = U.map (getMisoAngle symm q) ks2
  in U.minimum $ U.concatMap foo ks1

misoSingleORRef :: Vector OR -> Symm -> Quaternion -> Quaternion -> Double
misoSingleORRef ors symm q1 q2 = let
  ks = U.map ((q2 #<=) . qOR) ors
  in U.minimum $ U.map (getMisoAngle symm q1) ks