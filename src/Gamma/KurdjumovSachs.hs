{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
module Gamma.KurdjumovSachs
       ( ksTrans
       ) where

import           System.IO

import           Hammer.Math.Algebra
import           Hammer.Texture.Orientation
import           Hammer.Texture.Symmetry
import           Hammer.Texture.SphereProjection

-- ======================================================================================= 

-- | Kurdjumov-Sachs orientation relationship for phase transformation defined by 24
-- rotation operations.
ksTrans :: [Quaternion]
ksTrans = map (\(h,k,l,o) -> toQuaternion $ mkAxisPair (Vec3 h k l) (Deg o))
  [ ( 1, 1, 2, 90), ( 1, 1,-2, 90), ( 1,-1, 2, 90), ( 1,-1,-2, 90)
  , (-1, 1, 2, 90), (-1, 1,-2, 90), (-1,-1, 2, 90), (-1,-1,-2, 90)
  , ( 1, 2, 1, 90), ( 1, 2,-1, 90), ( 1,-2, 1, 90), ( 1,-2,-1, 90)
  , (-1, 2, 1, 90), (-1, 2,-1, 90), (-1,-2, 1, 90), (-1,-2,-1, 90)
  , ( 2, 1, 1, 90), ( 2, 1,-1, 90), ( 2,-1, 1, 90), ( 2,-1,-1, 90)
  , (-2, 1, 1, 90), (-2, 1,-1, 90), (-2,-1, 1, 90), (-2,-1,-1, 90)]

ksPoles :: (Rot o)=> o -> Vec3 -> [Normal3]
ksPoles g pole = let
  q     = toQuaternion g
  ts    = map (q #<=) ksTrans
  foo   = mkNormal . activeVecRotation pole
  in (foo q) : (map foo ts)


-- | Test KS transformation by plotting pole figures.
--   > showTest "../PlotScripts/polefigure.data" [Vec3 0 0 1, Vec3 0 1 0, Vec3 0 0 1]
showTest :: FilePath -> [Vec3] -> IO ()
showTest file poles = let
  g = mkEuler (Deg 0) (Deg 54.7) (Deg 45)
  out h (Vec2 x y) = hPutStrLn h $ show x ++ "   " ++ show y
  points           = map (map (so3ProjCoord . steroSO3Proj) . ksPoles g) poles
  writeSet h set   = do
    hPutStrLn h "\n\n\"pole\""
    mapM_ (out h) set
  in do
    h    <- openFile file WriteMode
    mapM_ (writeSet h) points
    hClose h
