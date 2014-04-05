{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

module Gamma.GammaFinder
       ( findGamma
       , findOR
       , getGamma
       , testGammaFit
       , testFindGamma
       , testMisFunc
       ) where

import qualified Data.Vector as V

import           Data.Vector         (Vector)
import           System.Random       (randomIO)

import           File.ANGReader
import           Texture.Orientation
import           Texture.Symmetry

import           Hammer.Math.Algebra
import           Hammer.Math.Optimum

import           Gamma.KurdjumovSachs

import           Debug.Trace
import           Hammer.Render.VTK.VTKRender
import           Texture.SH.HyperSphere

dbg a = trace (show a) a


newtype FZQuaternion = FZQuaternion {qFZ :: Quaternion}

getGamma :: EBSDdata -> Quaternion
getGamma EBSDdata{..} = let
  is = V.findIndices ((> 0.1) . ci) nodes
  qs = V.map (rotation . (nodes V.!)) is
  gamma = toFZ Cubic $ findGamma qs
  in trace (show $ testGammaFit gamma qs) gamma

getQinFZ :: Quaternion -> FZQuaternion
getQinFZ = FZQuaternion . toFZ Cubic

genTS :: Quaternion -> Vector Quaternion
genTS t = let
  (w, v) = splitQuaternion t
  vs = getAllSymmVec (getSymmOps Cubic) v
  in V.map (mergeQuaternion . ((,) w)) vs

findOR :: Quaternion -> Vector Quaternion -> Quaternion
findOR ga qs = let
  fzqs = V.map getQinFZ qs
  func v f = let
    t = toQuaternion $ mkUnsafeRodrigues (f v)
    in errorfunc ga (genTS t) fzqs
  foo v = let
    k  = 0.01
    x  = func v id
    d1 = (func v (&+ Vec3 k 0 0) - x) / k
    d2 = (func v (&+ Vec3 0 k 0) - x) / k
    d3 = (func v (&+ Vec3 0 0 k) - x) / k
    in (x, Vec3 d1 d2 d3)
  ks    = toQuaternion $ mkAxisPair (Vec3 1 1 2) (Deg 90)
  guess = rodriVec $ fromQuaternion ks
  in toQuaternion $ mkUnsafeRodrigues $ bfgs defaultBFGS foo guess

findGamma :: Vector Quaternion -> Quaternion
findGamma qs = let
  fzqs = V.map getQinFZ qs
  func v = let
    gamma = toQuaternion $ mkUnsafeRodrigues v
    in errorfunc gamma ksTrans fzqs
  foo v = let
    k  = 0.001
    x  = func v
    d1 = (func (v &+ Vec3 k 0 0) - func (v &- Vec3 k 0 0)) / (2*k)
    d2 = (func (v &+ Vec3 0 k 0) - func (v &- Vec3 0 k 0)) / (2*k)
    d3 = (func (v &+ Vec3 0 0 k) - func (v &- Vec3 0 0 k)) / (2*k)
    in (x, Vec3 d1 d2 d3)
  guess = rodriVec $ fromQuaternion $ hotStart fzqs
  in toQuaternion $ mkUnsafeRodrigues $ bfgs defaultBFGS foo guess

hotStart :: Vector FZQuaternion -> Quaternion
hotStart gms = let
  qs = V.fromList $
       [ toQuaternion (mkEuler (Deg phi1) (Deg phi) (Deg phi2))
       | phi1 <- [0.0, 3 .. 90]
       , phi  <- [0.0, 3 .. 90]
       , phi2 <- [0.0, 3 .. 90]
       ]
  func q = errorfunc q ksTrans gms
  i = V.minIndex $ V.map func qs
  in qs V.! i

errorfunc :: Quaternion-> Vector Quaternion -> Vector FZQuaternion -> Double
errorfunc ga ts gms1FZ = abs $ 1 - (total / n)
  where
    func gm1 gm2 = abs $ composeQ0 (invert gm2) (qFZ gm1)

    gms2   = V.map (toFZ Cubic . (ga #<=)) ts
    q0s    = V.map (\gm1 -> V.maximum $ V.map (func gm1) gms2) gms1FZ

    total = V.sum q0s
    n     = fromIntegral (V.length gms1FZ)

-- ================================= Test Function =======================================

testGammaFit :: Quaternion -> Vector Quaternion -> (Double, Double)
testGammaFit ga gs = let
  gms = V.map getQinFZ gs
  m1  = errorfunc ga ksTrans gms
  t   = toQuaternion $ mkAxisPair (Vec3 1 1 2) (Deg 90)
  m2  = errorfuncSlowButSure ga gs t
  in (m1, m2)

testMisFunc :: IO ()
testMisFunc = do
  a <- randomIO
  let gms = V.map (getQinFZ . (a #<=)) ksTrans
  print $ errorfunc a ksTrans gms

testFindOR :: IO ()
testFindOR = do
  a <- randomIO
  let
    t  = toQuaternion $ mkAxisPair (Vec3 1 1 2) (Deg 90)
    ts = genTS t
    ms = V.map (a #<=) ts
    t' = findOR a ms
  print t
  print t'

testFindGamma :: IO ()
testFindGamma = randomIO >>= plotErrFunc

plotErrFunc :: Quaternion -> IO ()
plotErrFunc a = let
  gms = V.map (a #<=) ksTrans
  fzqs = V.map getQinFZ gms
  (grid, vtk) = mkSO3 35 35 35
  es = V.map (\s -> errorfunc (so3ToQuaternion s) ksTrans fzqs) grid
  func i _ = es V.! i
  attr = mkPointAttr "Error function" func
  vtk' = addDataPoints vtk attr

  gi   = so3ToQuaternion $ grid V.! (V.minIndex es)
  in do
    let
      a'   = findGamma gms
      fa   = toFZ Cubic a
      fa'  = toFZ Cubic a'
      fgi  = toFZ Cubic gi
      test g1 g2 = (fromAngle $ Deg 3) > (abs $ getOmega (g1 -#- g2))
    print "=================="
    print $ "Expect: " ++ show fa
    print (test fa fa')
    print $ "from " ++ show fgi ++ " got: " ++ show fa'

    writeUniVTKfile ("/home/edgar/Desktop/SO3ErrFunc.vtu") False vtk'

    let vtk2 = renderSO3PointsVTK (V.map quaternionToSO3 $ V.fromList [gi, toFZ Cubic gi, a, a'])
    writeUniVTKfile ("/home/edgar/Desktop/SO3ErrFuncP.vtu") False vtk2

errorfuncSlowButSure :: Quaternion -> Vector Quaternion -> Quaternion -> Double
errorfuncSlowButSure ga gms t = 1 - (total / n)
  where
    os = V.map symmOp (getSymmOps Cubic)
    getMaxQ0 gm = let
      func on om = abs $ composeQ0 ((ga #<= on) -#- (gm #<= om)) t
      allQ0 = V.concatMap (\on -> V.map (func on) os) os
      in V.maximum allQ0

    total = V.sum $ V.map getMaxQ0 gms
    n     = fromIntegral (V.length gms)
