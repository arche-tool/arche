{-# LANGUAGE
    FlexibleInstances
  , ScopedTypeVariables
  , TypeSynonymInstances
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Utils where

import Control.Applicative
import Test.QuickCheck

import Arche.OR
import Linear.Vect
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