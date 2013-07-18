{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
module Gamma.KurdjumovSachs
       ( ksTrans
       ) where

import           Hammer.Math.Algebra
import           Hammer.Texture.Orientation
import           Hammer.Texture.Symmetry
import           Hammer.Texture.SphereProjection


t1 = let
  pg = Vec3 1 1 1
  pa = Vec3 1 1 0
  theta = angle pa pg
  axis  = pa &^ pg
  in mkAxisPair axis (Rad theta)

t2 = let
  dg = Vec3 (-1)  0  1
  da = Vec3   1 (-1) 1
  theta = angle da dg
  axis  = da &^ dg
  in mkAxisPair axis (Rad theta)

ksTrans :: [Quaternion]
ksTrans = map (\(h,k,l,o) -> toQuaternion $ mkAxisPair (Vec3 h k l) (Deg o))
  [ ( 1, 1, 2, 90), ( 1, 1,-2, 90), ( 1,-1, 2, 90), ( 1,-1,-2, 90)
  , (-1, 1, 2, 90), (-1, 1,-2, 90), (-1,-1, 2, 90), (-1,-1,-2, 90)
  , ( 1, 2, 1, 90), ( 1, 2,-1, 90), ( 1,-2, 1, 90), ( 1,-2,-1, 90)
  , (-1, 2, 1, 90), (-1, 2,-1, 90), (-1,-2, 1, 90), (-1,-2,-1, 90)
  , ( 2, 1, 1, 90), ( 2, 1,-1, 90), ( 2,-1, 1, 90), ( 2,-1,-1, 90)
  , (-2, 1, 1, 90), (-2, 1,-1, 90), (-2,-1, 1, 90), (-2,-1,-1, 90)]

testKS :: Vec3 -> [Vec2]
testKS pole = let
  q  = mkQuaternion $ Vec4 0 0 0 1
  ts = map (q +@>) ksTrans
  qPole  = mkNormal $ activeVecRotation pole q
  tPoles = map (mkNormal . activeVecRotation pole) ts
  in map (so3ProjCoord . steroSO3Proj) (qPole : tPoles)


showTest projs = let
  out (Vec2 x y) = putStrLn  $ show x ++ "   " ++ show y
  in mapM_ out projs
