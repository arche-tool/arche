{-# LANGUAGE
    ExistentialQuantification
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , TemplateHaskell
  , TypeFamilies
  #-}
module Gamma.OR
  ( -- * Product-Parent mismatch evaluation
    findGamma
  , findOR
  , findORFace
  , hotStartGamma
  , hotStartOR
  , hotStartTesseract
  , FitError (avgError, devError, maxError)
  , evalMisoOR
  , misoSingleOR
  , misoDoubleOR
  , misoDoubleKS
  , singleerrorfunc
  , weightederrorfunc
  , uniformerrorfunc
  , faceerrorfunc
  , gammaFinderKernel
  -- * Deconvolution
  , oneStepDeconvulition
    -- * Orientation Relationship
  , OR (..)
  , mkOR
  , ksORs
  , ksOR
  , genTS
  , QuaternionFZ (qFZ)
  , getQinFZ
  , convert
    -- * Test functions
  , testGammaFit
  , testFindGamma
  , testTesseractFitting
  , testMisoKS
  ) where

import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed.Deriving
import System.Random
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G

import Linear.Vect
import Hammer.Math.Optimum
import Hammer.VTK
import Texture.HyperSphere
import Texture.IsoSphere
import Texture.Kernel (gaussianKernel)
import Texture.Orientation
import Texture.ODF
import Texture.Symmetry
import Texture.TesseractGrid

-- =======================================================================================

newtype QuaternionFZ = QuaternionFZ {qFZ :: Quaternion} deriving (Show)

instance Monoid QuaternionFZ where
  mempty      = QuaternionFZ mempty
  mappend p q = fromQuaternion (qFZ p `mappend` qFZ q)

instance Group QuaternionFZ where
  invert = fromQuaternion . invert . qFZ

instance Rot QuaternionFZ where
  toQuaternion   = qFZ
  fromQuaternion = getQinFZ
  getOmega       = getOmega . qFZ

newtype OR = OR {qOR :: Quaternion} deriving (Show, Monoid, Group, Rot)

mkOR :: Vec3D -> Deg -> OR
mkOR v = OR . toQuaternion . mkAxisPair v

convert :: (Rot a, Rot b) => a -> b
convert = fromQuaternion . toQuaternion

getQinFZ :: Quaternion -> QuaternionFZ
getQinFZ = QuaternionFZ . toFZ Cubic

ksOR :: OR
ksOR = OR $ toQuaternion $ mkAxisPair (Vec3 1 1 2) (Deg 90)

ksORs :: Vector OR
ksORs = genTS ksOR

genTS :: OR -> Vector OR
genTS (OR t) = let
  (w, v) = splitQuaternion t
  vs = V.convert $ getAllSymmVec (getSymmOps Cubic) v
  in G.map (OR . mergeQuaternion . (,) w) vs

misoDoubleKS :: Symm -> Quaternion -> Quaternion -> Double
misoDoubleKS = misoDoubleOR ksORs

misoDoubleOR :: Vector OR -> Symm -> Quaternion -> Quaternion -> Double
misoDoubleOR ors symm q1 q2 = let
  ks1 = U.map ((q1 #<=) . qOR) ors
  ks2 = U.map ((q2 #<=) . qOR) ors
  -- Fully correct. Need prove that works!
  foo q = U.map (getMisoAngle symm q) ks2
  in U.minimum $ U.concatMap foo ks1

misoSingleOR :: Vector OR -> Symm -> Quaternion -> Quaternion -> Double
misoSingleOR ors symm q1 q2 = let
  ks = U.map ((q2 #<=) . qOR) ors
  -- Fully correct. Need prove that works!
  in U.minimum $ U.map (getMisoAngle symm q1) ks

data FitError
  = FitError
  { avgError :: Deg
  , devError :: Deg
  , maxError :: Deg
  } deriving (Show)

type ErrorFunc = Quaternion -> Vector OR -> FitError

evalMisoOR :: Vector OR -> (Quaternion, Int) -> (Quaternion, Int) -> Double
evalMisoOR ors (qa, pa) (qb, pb)
  | pa == pb  = misoDoubleOR ors Cubic qa qb
  | otherwise = misoSingleOR ors Cubic qa qb

-- | Evaluates the average angular error in rad between given parent and product
-- orientations and given orientation relationship. The list of products is given in the
-- fundamental zone.
faceerrorfunc :: Vector ((Quaternion, Int), (Quaternion, Int)) -> Vector OR -> FitError
faceerrorfunc ms ors = let
  n     = fromIntegral (G.length ms)
  errs  = G.map (uncurry (evalMisoOR ors)) ms
  avg   = G.sum errs / n
  diff  = G.map ((\x->x*x) . (-) avg) errs
  dev   = sqrt (G.sum diff / n)
  in FitError
     { avgError = toAngle avg
     , devError = toAngle dev
     , maxError = toAngle (G.maximum errs)
     }

deltaVec3 :: (Vec3D -> Double) -> Vec3D -> (Double, Vec3D)
deltaVec3 func v = let
  k  = 0.001
  x  = func v
  d1 = (func (v &+ Vec3 k 0 0) - func (v &- Vec3 k 0 0)) / (2*k)
  d2 = (func (v &+ Vec3 0 k 0) - func (v &- Vec3 0 k 0)) / (2*k)
  d3 = (func (v &+ Vec3 0 0 k) - func (v &- Vec3 0 0 k)) / (2*k)
  in (x, Vec3 d1 d2 d3)

findORFace :: Vector ((Quaternion, Int), (Quaternion, Int)) -> OR -> OR
findORFace qs t0 = let
  func = fromAngle . avgError . faceerrorfunc qs .
         genTS . OR . toQuaternion . mkUnsafeRodrigues
  guess = rodriVec $ fromQuaternion $ qOR t0
  in OR $ toQuaternion $ mkUnsafeRodrigues $ bfgs defaultBFGS (deltaVec3 func) guess

findOR :: ErrorFunc -> Quaternion -> OR -> OR
findOR errf ga t0 = let
  func v = let
    t = OR . toQuaternion $ mkUnsafeRodrigues v
    in fromAngle $ avgError $ errf ga (genTS t)
  guess = rodriVec $ fromQuaternion $ qOR t0
  in OR $ toQuaternion $ mkUnsafeRodrigues $ bfgs defaultBFGS (deltaVec3 func) guess

findGamma :: ErrorFunc -> Quaternion -> Vector OR -> Quaternion
findGamma errf q0 ors = let
  func v = let
    gamma = toQuaternion $ mkUnsafeRodrigues v
    in fromAngle $ avgError $ errf gamma ors
  guess = rodriVec $ fromQuaternion q0
  cfg   = BFGScfg { epsi = 1e-4, tol = 1e-4, niter = 200 }
  in toQuaternion $ mkUnsafeRodrigues $ bfgs cfg (deltaVec3 func) guess

hotStartGamma :: ErrorFunc -> Quaternion
hotStartGamma errf = let
  qs = V.fromList
       [ toQuaternion (mkEuler (Deg _phi1) (Deg _phi) (Deg _phi2))
       | _phi1 <- [0.0, 3 .. 90]
       , _phi  <- [0.0, 3 .. 90]
       , _phi2 <- [0.0, 3 .. 90]
       ]
  func q = fromAngle $ avgError $ errf q ksORs
  i = V.minIndex $ V.map func qs
  in qs V.! i

hotStartOR :: ErrorFunc -> Quaternion -> OR
hotStartOR errf q = let
  ks   = rodriVec $ fromQuaternion $ toQuaternion $ mkAxisPair (Vec3 1 1 2) (Deg 90)
  func = OR . toQuaternion . mkUnsafeRodrigues . (ks &+)
  ts = V.fromList
       [ func (Vec3 r1 r2 r3)
       | r1 <- [-0.2, 0.02 .. 0.2]
       , r2 <- [-0.2, 0.02 .. 0.2]
       , r3 <- [-0.2, 0.02 .. 0.2]
       ]
  foo t = fromAngle $ avgError $ errf q (genTS t)
  i = V.minIndex $ V.map foo ts
  in ts V.! i

singleerrorfunc :: QuaternionFZ -> Quaternion-> Vector OR -> (Deg, Int)
singleerrorfunc productQ parentQ ors = let
  func gm1 gm2 = abs $ composeQ0 (invert gm2) (qFZ gm1)
  toAng = toAngle . (2 *) . acosSafe
  qps   = G.map (toFZ Cubic . (parentQ #<=) . qOR) ors
  ps    = G.map (func productQ) qps
  imax  = G.maxIndex ps
  in (toAng (ps G.! imax), imax)

-- | Evaluates the average angular error in rad between given parent and product
-- orientations and given orientation relationship. The list of products is given in the
-- fundamental zone.
uniformerrorfunc :: Vector QuaternionFZ -> Quaternion -> Vector OR -> FitError
uniformerrorfunc ms gamma ors
  | G.null ms = FitError 0 0 0
  | otherwise = let
    n     = fromIntegral (G.length ms)
    errs  = G.map (\m -> unDeg $ fst $ singleerrorfunc m gamma ors) ms
    avg   = G.sum errs / n
    diff  = G.map ((\x->x*x) . (-) avg) errs
    dev   = sqrt (G.sum diff / n)
    in FitError
       { avgError = Deg avg
       , devError = Deg dev
       , maxError = Deg (G.maximum errs)
       }

-- | Evaluates the average angular error in rad between given parent and product
-- orientations and given orientation relationship. The list of products is given in the
-- fundamental zone.
weightederrorfunc :: Vector Double -> Vector QuaternionFZ -> Quaternion-> Vector OR -> FitError
weightederrorfunc ws ms gamma ors
  | G.null ws || G.null ms = FitError 0 0 0
  | otherwise = let
    errs = G.map (\m -> unDeg $ fst $ singleerrorfunc m gamma ors) ms
    wt   = G.sum ws
    wq   = G.zipWith (*) ws errs
    diff = G.map ((\x->x*x) . (-) avg) errs
    dev  = sqrt (G.sum (G.zipWith (*) ws diff) / wt)
    avg  = G.sum wq / wt
    in FitError
       { avgError = Deg avg
       , devError = Deg dev
       , maxError = Deg (G.maximum wq / wt)
       }

-- ================================= Gamma finder width kernel ===========================

-- | Generate a PPODF (Possible Parent Orientation Function) using another ODF grid structure
-- for effeciecy sake. It also takes in consideranting both remaining paraents and products
-- orientation.
genPPODF :: ODF -> Vector OR -> Vector QuaternionFZ -> Vector QuaternionFZ -> ODF
genPPODF odf ors rgs ms = addPoints (U.map qFZ rgs U.++ gs) (resetODF odf)
  where
    func m = U.map (toFZ Cubic . (m #<=) . qOR) ors
    gs     = U.concatMap (func . qFZ) ms

gammaFinderKernel :: ODF -> Vector OR -> Vector QuaternionFZ
                  -> Vector QuaternionFZ -> (Quaternion, FitError)
gammaFinderKernel odf ors rgs ms = (q, err)
  where
    odf1   = genPPODF odf ors rgs ms
    (q, _) = getMaxOrientation odf1
    err    = uniformerrorfunc ms q ors

-- | Decompose an ODF in isotropic Gaussians
oneStepDeconvulition :: ODF -> (Quaternion, Double, Rad, ODF)
oneStepDeconvulition odf = (q, i, w, odf1)
  where
    -- TODO fit w
    w = findBestGaussianFit odf q i
    (q, i) = getMaxOrientation odf
    odf1   = addPointsWithConst (U.singleton q) (-i) (Just w) odf

fitGaussianWidth :: ODF -> Quaternion -> Double -> Rad -> Double
fitGaussianWidth odf q0 scale w = integrateODFwith (\q k -> sq (k - scale * gauss q)) odf
  where
    gauss = gaussianKernel w . composeQ0 q0
    sq x = x * x

findBestGaussianFit :: ODF -> Quaternion -> Double -> Rad
findBestGaussianFit odf q0 k0 = toAngle $ ds U.! i
 where
   i = U.minIndex $ U.map func ds
   ds = U.fromList [1, 2, 3, 4, 5]
   func = fitGaussianWidth odf q0 k0 . Rad . fromAngle . Deg

-- ===================================== Teseeract binning ========================================

hotStartTesseract :: Vector OR -> Vector QuaternionFZ -> (Quaternion, FitError, TesseractGrid Double)
hotStartTesseract ors ms
  | avgError ermax > avgError eravg = (gavg, eravg, tess)
  | otherwise                       = (gmax, ermax, tess)
  where
    func m = U.map (toFZ Cubic . (m #<=) . qOR) ors
    grid  = 30
    range = 4 / fromIntegral grid
    gs    = U.concatMap (func . qFZ) ms
    t0    = emptyTesseract grid 0
    tess  = binningTesseract (U.convert gs) t0
    gmax  = tesseractToQuaternion $ maxTesseractPoint tess
    gc    = U.filter ((> range) . getMisoAngle Cubic gmax) gs
    gavg  = averageQuaternionWithSymm Cubic (V.convert gc :: V.Vector Quaternion)
    eravg = uniformerrorfunc ms gavg ors
    ermax = uniformerrorfunc ms gmax ors

-- ======================================= Unbox ==========================================

derivingUnbox "QuaternionFZ"
    [t| QuaternionFZ -> Quaternion |]
    [| \(QuaternionFZ q) -> q |]
    [| QuaternionFZ |]

derivingUnbox "OR"
    [t| OR -> Quaternion |]
    [| \(OR q) -> q |]
    [| OR |]

-- ================================= Test Function =======================================

testGammaFit :: Quaternion -> Vector Quaternion -> OR -> (FitError, FitError)
testGammaFit ga gs t = let
  gms = G.map getQinFZ gs
  m1  = uniformerrorfunc gms ga (genTS t)
  m2  = errorfuncSlowButSure ga gs (qOR t)
  in (m1, m2)

testFindOR :: IO ()
testFindOR = do
  a <- randomIO
  let
    t  = mkOR (Vec3 1 1 2) (Deg 90)
    ts = genTS t
    ms = G.map ((a #<=) . qOR) ts
    t' = findOR errf a t
    errf = uniformerrorfunc fzqs
    fzqs = G.map getQinFZ ms
  print (convert t  :: AxisPair)
  print (convert t' :: AxisPair)
  print ((convert $ hotStartOR errf a) :: AxisPair)

testFindGamma :: IO ()
testFindGamma = randomIO >>= plotErrFunc

plotErrFunc :: Quaternion -> IO ()
plotErrFunc a = let
  gms  = G.map ((a #<=) . qOR) ksORs
  fzqs = G.map getQinFZ gms
  errf = uniformerrorfunc fzqs
  (grid, vtk) = mkSO3 35 35 35
  es = G.map (\s -> fromAngle $ avgError $ uniformerrorfunc fzqs (so3ToQuaternion s) ksORs) grid
  attr = mkPointAttr "Error function" (es U.!)
  vtk' = addPointAttr vtk attr

  gi   = so3ToQuaternion $ grid U.! (U.minIndex es)
  in do
    let
      a'   = findGamma errf mempty ksORs
      fa   = toFZ Cubic a
      fa'  = toFZ Cubic a'
      fgi  = toFZ Cubic gi
      test g1 g2 = fromAngle (Deg 3) > abs (getOmega (g1 -#- g2))
    print "=================="
    print $ "Expect: " ++ show fa
    print (test fa fa')
    print $ testGammaFit a' gms ksOR
    print $ "from " ++ show fgi ++ " got: " ++ show fa'

    writeUniVTKfile "/home/edgar/Desktop/SO3ErrFunc.vtu" False vtk'

    let vtk2 = renderSO3PointsVTK (U.map quaternionToSO3 $ U.fromList [gi, toFZ Cubic gi, a, a'])
    writeUniVTKfile "/home/edgar/Desktop/SO3ErrFuncP.vtu" False vtk2

errorfuncSlowButSure :: Quaternion -> Vector Quaternion -> Quaternion -> FitError
errorfuncSlowButSure ga gms t = let
  os = U.map symmOp (getSymmOps Cubic)
  getMaxQ0 gm = let
    func on om = abs $ composeQ0 ((ga #<= on) -#- (gm #<= om)) t
    allQ0 = U.concatMap (\on -> U.map (func on) os) os
    in U.maximum allQ0
  toAng = toAngle . (2 *) . acosSafe
  q0s   = G.map getMaxQ0 gms
  n     = fromIntegral (G.length gms)
  dev   = let s = G.map ((\x->x*x) . (-) avg) q0s in sqrt (abs $ 1 - G.sum s/n)
  avg   = G.sum q0s / n
  in FitError
     { avgError = toAng avg
     , devError = toAng dev
     , maxError = toAng (G.minimum q0s)
     }

testMisoKS :: Vector Quaternion -> IO Deg
testMisoKS ks = do
  a  <- randomIO
  i1 <- randomRIO (0, U.length ks - 1)
  i2 <- randomRIO (0, U.length ks - 1)
  let
    ks1 = ks U.! i1
    ks2 = ks U.! i2
    m1  = a #<= ks1
    m2  = a #<= ks2
    miso = getMisoAngle Cubic m1 m2
  return (toAngle miso :: Deg)

testAvg :: Int -> IO ()
testAvg n = do
  gen <- newStdGen
  let
    vs = take n $ randoms gen :: [Vec3D]
    q  = toQuaternion $ mkEuler (Deg 10) (Deg 15) (Deg 2)
    rs = map (\v -> toQuaternion $ mkAxisPair v (Deg 1)) vs
    qs = map (q #<=) rs
  print q
  print $ averageQuaternion qs

applyDeviation :: Deg -> Quaternion -> IO Quaternion
applyDeviation w q = do
  v <- randomIO
  let d = toQuaternion $ mkAxisPair v w
  return (q #<= d)

testPPODF :: FilePath -> IO ()
testPPODF dir = do
  qg1 <- randomIO
  qg2 <- applyDeviation (Deg 15) qg1
  ors <- U.mapM (fmap OR . applyDeviation (Deg 4) . qOR) ksORs
  let
    gamma1 = qFZ $ getQinFZ qg1
    gamma2 = qFZ $ getQinFZ qg2
    ms1    = G.map (getQinFZ . (gamma1 #<=) . toQuaternion) ors
    ms2    = G.map (getQinFZ . (gamma2 #<=) . toQuaternion) ors
    odf0   = buildEmptyODF 3 Cubic (Deg 5)
    ppodf0 = genPPODF odf0 ors U.empty (ms1 G.++ ms2)
    (g1, i1, _, ppodf1) = oneStepDeconvulition ppodf0
    (g2, i2, _, ppodf2) = oneStepDeconvulition ppodf1
    (g3, i3, _, ppodf3) = oneStepDeconvulition ppodf2
    ps = U.fromList [gamma1, gamma2, g1, g2, g3]
  print $ uniformerrorfunc ms1 gamma1 ksORs
  print $ uniformerrorfunc ms2 gamma2 ksORs
  print (gamma1, gamma2, g1, g2, g3)
  print (i1, i2, i3)
  writeUniVTKfile (dir ++ "/tess-fittest-ODF0.vtu"  ) True (renderODFVTK ppodf0)
  writeUniVTKfile (dir ++ "/tess-fittest-ODF1.vtu"  ) True (renderODFVTK ppodf1)
  writeUniVTKfile (dir ++ "/tess-fittest-ODF2.vtu"  ) True (renderODFVTK ppodf2)
  writeUniVTKfile (dir ++ "/tess-fittest-ODF3.vtu"  ) True (renderODFVTK ppodf3)
  writeUniVTKfile (dir ++ "/tess-fittest-Points.vtu") True (renderQuaternions ps [])

testTesseractFitting :: FilePath -> IO ()
testTesseractFitting dir = do
  qg  <- randomIO
  ors <- U.mapM (applyDeviation (Deg 5) . qOR) ksORs
  let
    gamma = qFZ $ getQinFZ qg
    ms    = G.map (getQinFZ . (gamma #<=) . toQuaternion) (U.map OR ors)
    (gt, et, tt)  = hotStartTesseract ksORs ms
    --(gf, ef, orf) = getWGammaOR       ksOR ws (G.map qFZ ms)
    ps = U.fromList [gamma, gt]
  print $ uniformerrorfunc ms gamma ksORs
  print gamma
  print (gt, et)
  --print $ (gf, ef)
  writeUniVTKfile (dir ++ "/tess-fittest.vti") True (plotTesseract tt)
  writeUniVTKfile (dir ++ "/tess-fittest.vtu") True (plotTesseractPoints ps)
