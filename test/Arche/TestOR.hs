{-# LANGUAGE
    FlexibleInstances
  , ScopedTypeVariables
  , TypeSynonymInstances
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Arche.TestOR
  ( test
  ) where

import Control.Applicative
import Data.Monoid ((<>), mempty)
import Data.Vector.Unboxed (Vector)
import Test.QuickCheck
import Test.Tasty

import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed   as U
import qualified Test.Tasty.HUnit      as HU
import qualified Test.Tasty.QuickCheck as QC

import Arche.OR
import Linear.Vect
import Linear.Mat
import Texture.Orientation
import Texture.Symmetry

-- ================================================================================

instance Arbitrary Euler where
  arbitrary = liftA3 mkEuler x2 x1 x2
    where
      x1 = Deg <$> choose (0, 180)
      x2 = Deg <$> choose (0, 360)

instance Arbitrary Vec3D where
  arbitrary = normalize <$> liftA3 Vec3 p p p
    where p = choose (0,1)

instance Arbitrary Quaternion where
  arbitrary = toQuaternion <$> (arbitrary :: Gen Euler)

instance Arbitrary Rodrigues where
  arbitrary = fromQuaternion <$> arbitrary

instance Arbitrary RotMatrix where
  arbitrary = fromQuaternion <$> arbitrary

instance Arbitrary AxisPair where
  arbitrary = fromQuaternion <$> arbitrary

instance Arbitrary Deg where
  arbitrary = Deg <$> arbitrary

instance Arbitrary OR where
  arbitrary = OR <$> arbitrary


(~=) :: (NearZero a, Ord a, Num a) => a -> a -> Bool
a ~= b = isMainlyZero $ a - b

msgFail :: (Show a, Testable prop)=> a -> prop -> Property
msgFail text = counterexample ("\x1b[7m Fail: " ++ show text ++ "! \x1b[0m")

omegaDeg :: Rot q => q -> q -> Deg
omegaDeg a b = toAngle . getOmega $ a -@- b :: Deg

omegaSymmDeg :: Rot q => Symm -> q -> q -> Deg
omegaSymmDeg symm a b = toAngle $ getMisoAngle symm (toQuaternion a) (toQuaternion b)

binaryTest :: (Show a, Show b) => String -> (a -> b -> Bool) -> a -> b -> Property
binaryTest msg test a b = counterexample (msg ++ ": " ++ show a ++ " " ++ show b) $ a `test` b

-- ================================================================================

test :: TestTree
test = testGroup "OR"
  [ QC.testProperty "misoSingleOR" testMisoSingleOR
  , QC.testProperty "misoDoubleOR" testMisoDoubleOR
  , QC.testProperty "misoDoubleORTransitive"   testMisoDoubleORTransitive
  ]

testMisoSingleOR :: OR -> Quaternion -> Quaternion -> Property
testMisoSingleOR or q1 q2 = let
  ors = genTS or
  symOps = getSymmOps Cubic
  in binaryTest "matchRef" (~=) (misoSingleOR2 ors Cubic q1 q2) (misoSingleORRef ors Cubic q1 q2)

testMisoDoubleOR :: OR -> Quaternion -> Quaternion -> Property
testMisoDoubleOR or q1 q2 = let
  ors = genTS or
  symOps = getSymmOps Cubic
  in binaryTest "matchRef" (~=) (misoDoubleOR2 ors Cubic q1 q2) (misoDoubleORRef ors Cubic q1 q2)

testMisoDoubleORTransitive :: OR -> Quaternion -> Quaternion -> Property
testMisoDoubleORTransitive  or q1 q2 = let
  ors = genTS or
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

misoDoubleOR2 :: Vector OR -> Symm -> Quaternion -> Quaternion -> Double
misoDoubleOR2 ors symOps q1 q2 = let
  -- Fully correct. Need prove that works!
  lor = U.toList ors
  func :: OR -> OR -> Double
  func ora orb = let
    ks1 = ((q1 #<=) . qOR) ora
    ks2 = ((q2 #<=) . qOR) orb
    in getMisoAngle symOps ks1 ks2
  in L.minimum $ [ func ora orb | ora <- lor, orb <- lor ]

misoSingleOR2 :: Vector OR -> Symm -> Quaternion -> Quaternion -> Double
misoSingleOR2 ors symOps q1 q2 = let
  ks = U.map (getMisoAngle symOps q1 . (q2 #<=) . qOR) ors
  -- Fully correct. Need prove that works!
  in U.minimum ks