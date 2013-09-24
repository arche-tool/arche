{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ExistentialQuantification #-}
module Gamma.KurdjumovSachs
       ( ksTrans
       , ksQuaters
       , ksOris
       , ksPoles
       ) where

import qualified Data.Vector as V

import           Data.Vector (Vector)

import           System.IO

import           Hammer.Math.Algebra
import           Hammer.Texture.Orientation
import           Hammer.Texture.Symmetry
import           Hammer.Texture.SphereProjection

-- ======================================================================================= 

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
  g      = mkEuler (Deg 0) (Deg 54.7) (Deg 45)
  foo    = V.map (so3ProjCoord . steroSO3Proj) . ksPoles g
  points = map foo poles
  writeSet h set = do
    hPutStrLn h "\n\n\"pole\""
    V.mapM_ (out h) set
  in do
    h <- openFile file WriteMode
    mapM_ (writeSet h) points
    hClose h
