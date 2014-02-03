{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ExistentialQuantification #-}
module Gamma.KurdjumovSachs
       ( ksTrans
       , ksQuaters
       , ksOris
       , ksPoles
       , misoKS
       ) where

import qualified Data.Vector   as V

import           Data.Vector   (Vector)
import           Control.Monad (replicateM_)

import           System.IO
import           System.Random

import           Hammer.Math.Algebra
import           Texture.Orientation
import           Texture.Symmetry
import           Texture.SphereProjection

testKS = let
  q = toQuaternion $ mkAxisPair (Vec3 1 1 2) (Deg 90)
  in V.map ((q #<=) . symmOp) (getSymmOps Cubic)

testAll dir w file n = do
  h <- openFile file WriteMode
  let
    q  = toQuaternion $ mkAxisPair dir w
    ks = V.map (#<= q) ksTrans
  replicateM_ n (testKSMiso ks >>= (hPutStrLn h . show . unDeg))
  hClose h

testKSMiso :: Vector Quaternion -> IO Deg
testKSMiso ks = do
  a  <- randomIO
  i1 <- randomRIO (0, V.length ks - 1)
  i2 <- randomRIO (0, V.length ks - 1)
  let
    ks1 = ks V.! i1
    ks2 = ks V.! i2
    m1  = a #<= ks1
    m2  = a #<= ks2
    miso = getMisoAngle Cubic m1 m2
  return (toAngle miso :: Deg)

-- ======================================================================================= 

misoKS :: Symm -> Quaternion -> Quaternion -> Double
misoKS symm q1 q2 = let
  ks1 = ksQuaters q1
  ks2 = ksQuaters q2
  foo q = V.map (getMisoAngle symm q) ks2
  in V.minimum $ V.concatMap foo ks1

-- | Kurdjumov-Sachs orientation relationship for phase transformation defined by 24
-- rotation operations.
ksTrans :: Vector Quaternion
ksTrans = let
  foo (h,k,l,o) = toQuaternion $ mkAxisPair (Vec3 h k l) (Deg o)
  ks = [ ( 1, 1, 2, 90), ( 1, 1,-2, 90), ( 1,-1, 2, 90), ( 1,-1,-2, 90)
       , (-1, 1, 2, 90), (-1, 1,-2, 90), (-1,-1, 2, 90), (-1,-1,-2, 90)
       , ( 1, 2, 1, 90), ( 1, 2,-1, 90), ( 1,-2, 1, 90), ( 1,-2,-1, 90)
       , (-1, 2, 1, 90), (-1, 2,-1, 90), (-1,-2, 1, 90), (-1,-2,-1, 90)
       , ( 2, 1, 1, 90), ( 2, 1,-1, 90), ( 2,-1, 1, 90), ( 2,-1,-1, 90)
       , (-2, 1, 1, 90), (-2, 1,-1, 90), (-2,-1, 1, 90), (-2,-1,-1, 90)]
  in V.map foo (V.fromList ks)

ksOris :: (Rot o)=> o -> Vector o
ksOris g = V.map ((g #<=) . fromQuaternion) ksTrans

ksQuaters :: Quaternion -> Vector Quaternion
ksQuaters q = V.map (q #<=) ksTrans

ksPoles :: (Rot o)=> o -> Vec3 -> Vector Normal3
ksPoles g pole = let
  q     = toQuaternion g
  ts    = ksQuaters q
  foo   = mkNormal . activeVecRotation pole
  in (foo q) `V.cons` (V.map foo ts)


-- | Test KS transformation by plotting pole figures.
--   > showTest "../PlotScripts/polefigure.data" [Vec3 0 0 1, Vec3 0 1 0, Vec3 0 0 1]
showTest :: FilePath -> [Vec3] -> IO ()
showTest file poles = let
  out h (Vec2 x y) = hPutStrLn h $ show x ++ "   " ++ show y
  g      = mkEuler (Deg 0) (Deg 0) (Deg 0)
  foo    = V.map (so3ProjCoord . steroSO3Proj) . ksPoles g
  points = map foo poles
  writeSet h set = do
    hPutStrLn h "\n\n\"pole\""
    V.mapM_ (out h) set
  in do
    h <- openFile file WriteMode
    mapM_ (writeSet h) points
    hClose h
