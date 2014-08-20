{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Gamma.OR
       ( -- * Product-Parent mismatch evaluation
         findGamma
       , findGammaOR
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
         -- * Orientation Relationship
       , OR (..)
       , mkOR
       , ksORs
       , ksOR
       , genTS
       , QuaternionFZ (qFZ)
       , getQinFZ
       , convert
       , shitQAvg
         -- * Test functions
       , testGammaFit
       , testFindGamma
       , testTesseractFitting
       , testMisoKS
       ) where

import qualified Data.Packed                 as HV
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Base    as GB
import qualified Data.Vector.Generic.Mutable as GM

import           Data.Vector.Unboxed (Vector)
import           Numeric.Container   (add, sub)
import           Control.Monad       (liftM)

import           System.Random

import           Numeric.GSL.Minimization

import           Texture.Orientation
import           Texture.Symmetry

import           Hammer.Math.Algebra
import           Hammer.Math.Optimum

import           Hammer.VTK
import           Texture.HyperSphere
import           Texture.TesseractGrid

import           Debug.Trace
dbg a = trace (show a) a

-- ======================================================================================= 

newtype QuaternionFZ = QuaternionFZ {qFZ :: Quaternion} deriving (Show)
newtype OR = OR {qOR :: Quaternion} deriving (Show)

instance Rot QuaternionFZ where
  p #<= q        = QuaternionFZ $ (qFZ p) #<= (qFZ q)
  invert         = QuaternionFZ . invert . qFZ
  toQuaternion   = qFZ
  fromQuaternion = getQinFZ
  zerorot        = QuaternionFZ zerorot
  getOmega       = getOmega . qFZ

instance Rot OR where
  (OR p) #<= (OR q) = OR $ p #<= q
  invert         = OR . invert . qOR
  toQuaternion   = qOR
  fromQuaternion = OR
  zerorot        = OR zerorot
  getOmega       = getOmega . qOR

mkOR :: Vec3 -> Deg -> OR
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
  in G.map (OR . mergeQuaternion . ((,) w)) vs

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
  errs  = G.map (\(q1,q2) -> evalMisoOR ors q1 q2) ms
  avg   = G.sum errs / n
  diff  = G.map ((\x->x*x) . ((-) avg)) errs
  dev   = sqrt (G.sum diff / n)
  in FitError
     { avgError = toAngle avg
     , devError = toAngle dev
     , maxError = toAngle (G.maximum errs)
     }

findORFace :: Vector ((Quaternion, Int), (Quaternion, Int)) -> OR -> OR
findORFace qs t0 = let
  func = fromAngle . avgError . faceerrorfunc qs .
         genTS . OR . toQuaternion . mkUnsafeRodrigues
  foo v = let
    k  = 0.001
    x  = func v
    d1 = (func (v &+ Vec3 k 0 0) - func (v &- Vec3 k 0 0)) / (2*k)
    d2 = (func (v &+ Vec3 0 k 0) - func (v &- Vec3 0 k 0)) / (2*k)
    d3 = (func (v &+ Vec3 0 0 k) - func (v &- Vec3 0 0 k)) / (2*k)
    in (x, Vec3 d1 d2 d3)
  guess = rodriVec $ fromQuaternion $ qOR t0
  in OR $ toQuaternion $ mkUnsafeRodrigues $ bfgs defaultBFGS foo guess

findOR :: ErrorFunc -> Quaternion -> OR -> OR
findOR errf ga t0 = let
  func v = let
    t = OR . toQuaternion $ mkUnsafeRodrigues v
    in fromAngle $ avgError $ errf ga (genTS t)
  foo v = let
    k  = 0.001
    x  = func v
    d1 = (func (v &+ Vec3 k 0 0) - func (v &- Vec3 k 0 0)) / (2*k)
    d2 = (func (v &+ Vec3 0 k 0) - func (v &- Vec3 0 k 0)) / (2*k)
    d3 = (func (v &+ Vec3 0 0 k) - func (v &- Vec3 0 0 k)) / (2*k)
    in (x, Vec3 d1 d2 d3)
  guess = rodriVec $ fromQuaternion $ qOR t0
  in OR $ toQuaternion $ mkUnsafeRodrigues $ bfgs defaultBFGS foo guess

findGamma :: ErrorFunc -> Quaternion -> Vector OR -> Quaternion
findGamma errf q0 ors = let
  func v = let
    gamma = toQuaternion $ mkUnsafeRodrigues v
    in fromAngle $ avgError $ errf gamma ors
  foo v = let
    k  = 0.001
    x  = func v
    d1 = (func (v &+ Vec3 k 0 0) - func (v &- Vec3 k 0 0)) / (2*k)
    d2 = (func (v &+ Vec3 0 k 0) - func (v &- Vec3 0 k 0)) / (2*k)
    d3 = (func (v &+ Vec3 0 0 k) - func (v &- Vec3 0 0 k)) / (2*k)
    in (x, Vec3 d1 d2 d3)
  guess = rodriVec $ fromQuaternion q0
  cfg   = BFGScfg { epsi = 1e-4, tol = 1e-4, niter = 200 }
  in toQuaternion $ mkUnsafeRodrigues $ bfgs cfg foo guess

findGammaOR :: ErrorFunc -> Quaternion -> OR -> (Quaternion, OR)
findGammaOR errf q0 or0 = let
  func v = let
    [x1,x2,x3, k1, k2, k3] = HV.toList v
    g = toQuaternion $ mkUnsafeRodrigues (Vec3 x1 x2 x3)
    t = OR $ toQuaternion $ mkUnsafeRodrigues (Vec3 k1 k2 k3)
    in fromAngle $ avgError $ errf g (genTS t)
  foo v = let
    d1 = HV.fromList [k, 0, 0, 0, 0, 0]
    d2 = HV.fromList [0, k, 0, 0, 0, 0]
    d3 = HV.fromList [0, 0, k, 0, 0, 0]
    d4 = HV.fromList [0, 0, 0, k, 0, 0]
    d5 = HV.fromList [0, 0, 0, 0, k, 0]
    d6 = HV.fromList [0, 0, 0, 0, 0, k]
    k  = 0.001
    dr1 = (func (v `add` d1) - func (v `sub` d1)) / (2*k)
    dr2 = (func (v `add` d2) - func (v `sub` d2)) / (2*k)
    dr3 = (func (v `add` d3) - func (v `sub` d3)) / (2*k)

    dt1 = (func (v `add` d4) - func (v `sub` d4)) / (2*k)
    dt2 = (func (v `add` d5) - func (v `sub` d5)) / (2*k)
    dt3 = (func (v `add` d6) - func (v `sub` d6)) / (2*k)
    in (HV.fromList [dr1, dr2, dr3, dt1, dt2, dt3])
  Vec3 ra rb rc = rodriVec $ convert q0
  Vec3 ta tb tc = rodriVec $ convert or0
  guess = HV.fromList [ra, rb, rc, ta, tb, tc]
  (r,_) = minimizeVD VectorBFGS2 0.00001 200 0.01 0.1 func foo guess
  --box = HV.fromList [5,5,5,5,5,5]
  --(r,_) = minimizeV NMSimplex2 0.00001 200 box func guess
  [r1, r2, r3, t1, t2, t3] = HV.toList r
  gf = convert $ mkUnsafeRodrigues (Vec3 r1 r2 r3)
  tf = convert $ mkUnsafeRodrigues (Vec3 t1 t2 t3)
  in (gf, tf)

-- $ hotStart fzqs
-- $ qOR $ mkOR (Vec3 1 1 2) (Deg 90)

hotStartGamma :: ErrorFunc -> Quaternion
hotStartGamma errf = let
  qs = V.fromList $
       [ toQuaternion (mkEuler (Deg phi1) (Deg phi) (Deg phi2))
       | phi1 <- [0.0, 3 .. 90]
       , phi  <- [0.0, 3 .. 90]
       , phi2 <- [0.0, 3 .. 90]
       ]
  func q = fromAngle $ avgError $ errf q ksORs
  i = V.minIndex $ V.map func qs
  in qs V.! i

hotStartOR :: ErrorFunc -> Quaternion -> OR
hotStartOR errf q = let
  ks   = rodriVec $ fromQuaternion $ toQuaternion $ mkAxisPair (Vec3 1 1 2) (Deg 90)
  func = OR . toQuaternion . mkUnsafeRodrigues . (ks &+)
  ts = V.fromList $
       [ func (Vec3 r1 r2 r3)
       | r1 <- [-0.2, 0.02 .. 0.2]
       , r2 <- [-0.2, 0.02 .. 0.2]
       , r3 <- [-0.2, 0.02 .. 0.2]
       ]
  foo t = fromAngle $ avgError $ errf q (genTS t)
  i = V.minIndex $ V.map foo ts
  in ts V.! i

singleerrorfunc :: QuaternionFZ -> Quaternion-> Vector OR -> Deg
singleerrorfunc productQ parentQ ors = let
  toAng = toAngle . (2 *) . acosSafe
  func gm1 gm2 = abs $ composeQ0 (invert gm2) (qFZ gm1)
  qps = G.map (toFZ Cubic . (parentQ #<=) . qOR) ors
  in toAng $ G.maximum $ G.map (func productQ) qps

-- | Evaluates the average angular error in rad between given parent and product
-- orientations and given orientation relationship. The list of products is given in the
-- fundamental zone.
uniformerrorfunc :: Vector QuaternionFZ -> Quaternion-> Vector OR -> FitError
uniformerrorfunc ms gamma ors
  | G.null ms = FitError 0 0 0
  | otherwise = let
    n     = fromIntegral (G.length ms)
    errs  = G.map (\m -> unDeg $ singleerrorfunc m gamma ors) ms
    avg   = G.sum errs / n
    diff  = G.map ((\x->x*x) . ((-) avg)) errs
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
    errs = G.map (\m -> unDeg $ singleerrorfunc m gamma ors) ms
    wt   = G.sum ws
    wq   = G.zipWith (*) ws errs
    diff = G.map ((\x->x*x) . ((-) avg)) errs
    dev  = sqrt (G.sum (G.zipWith (*) ws diff) / wt)
    avg  = G.sum wq / wt
    in FitError
       { avgError = Deg avg
       , devError = Deg dev
       , maxError = Deg (G.maximum wq / wt)
       }

shitQAvg :: Vector Quaternion -> Quaternion
shitQAvg vq = U.foldl func (U.head vq) (U.tail vq)
  where
    os = getSymmOps Cubic
    func avg q = let
      qs = U.map ((q #<=) . symmOp) os
      ms = U.map (composeQ0 avg) qs
      i  = U.minIndex ms
      vi = quaterVec (qs U.! i)
      in mkQuaternion $ vi &+ (quaterVec avg)

-- ===================================== Teseeract binning ===============================

hotStartTesseract :: Vector OR -> Vector QuaternionFZ -> (Quaternion, FitError, TesseractGrid Double)
hotStartTesseract ors ms
  | avgError ermax > avgError eravg = (gavg, eravg, tess)
  | otherwise                       = (gmax, ermax, tess)
  where
    func m = U.map (toFZ Cubic . (m #<=) . qOR) ors
    grid  = 30
    range = 4 / (fromIntegral grid)
    gs    = U.concatMap (func . qFZ) ms
    t0    = emptyTesseract grid 0
    tess  = binningTesseract (U.convert gs) t0
    gmax  = tesseractToQuaternion $ maxTesseractPoint tess
    gc    = U.filter ((> range) . getMisoAngle Cubic gmax) gs
    gavg  = shitQAvg gc
    eravg = uniformerrorfunc ms gavg ors
    ermax = uniformerrorfunc ms gmax ors

-- =========================================== Unbox QuaternionFZ  =================================

newtype instance U.MVector s QuaternionFZ = MV_FZ (U.MVector s Quaternion)
newtype instance U.Vector    QuaternionFZ = V_FZ  (U.Vector    Quaternion)

instance U.Unbox QuaternionFZ

instance GM.MVector U.MVector QuaternionFZ where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_FZ v)                         = GM.basicLength v
  basicUnsafeSlice i n (MV_FZ v)                = MV_FZ $ GM.basicUnsafeSlice i n v
  basicOverlaps (MV_FZ v1) (MV_FZ v2)           = GM.basicOverlaps v1 v2
  basicUnsafeNew n                              = MV_FZ `liftM` GM.basicUnsafeNew n
  basicUnsafeReplicate n (QuaternionFZ x)       = MV_FZ `liftM` GM.basicUnsafeReplicate n x
  basicUnsafeRead (MV_FZ v) i                   = GM.basicUnsafeRead v i >>= (return . QuaternionFZ)
  basicUnsafeWrite (MV_FZ v) i (QuaternionFZ x) = GM.basicUnsafeWrite v i x
  basicClear (MV_FZ v)                          = GM.basicClear v
  basicSet (MV_FZ v) (QuaternionFZ x)           = GM.basicSet v x
  basicUnsafeCopy (MV_FZ v1) (MV_FZ v2)         = GM.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MV_FZ v) n                   = MV_FZ `liftM` GM.basicUnsafeGrow v n

instance GB.Vector U.Vector QuaternionFZ where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_FZ v)         = V_FZ `liftM` GB.basicUnsafeFreeze v
  basicUnsafeThaw (V_FZ v)            = MV_FZ `liftM` GB.basicUnsafeThaw v
  basicLength (V_FZ v)                = GB.basicLength v
  basicUnsafeSlice i n (V_FZ v)       = V_FZ $ GB.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_FZ v) i        = GB.basicUnsafeIndexM v i >>= (return . QuaternionFZ)
  basicUnsafeCopy (MV_FZ mv) (V_FZ v) = GB.basicUnsafeCopy mv v
  elemseq _ (QuaternionFZ x) t        = GB.elemseq (undefined :: Vector a) x t

-- =========================================== Unbox OR =================================

newtype instance U.MVector s OR = MV_OR (U.MVector s Quaternion)
newtype instance U.Vector    OR = V_OR  (U.Vector    Quaternion)

instance U.Unbox OR

instance GM.MVector U.MVector OR where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_OR v)                 = GM.basicLength v
  basicUnsafeSlice i n (MV_OR v)        = MV_OR $ GM.basicUnsafeSlice i n v
  basicOverlaps (MV_OR v1) (MV_OR v2)   = GM.basicOverlaps v1 v2
  basicUnsafeNew n                      = MV_OR `liftM` GM.basicUnsafeNew n
  basicUnsafeReplicate n (OR x)         = MV_OR `liftM` GM.basicUnsafeReplicate n x
  basicUnsafeRead (MV_OR v) i           = GM.basicUnsafeRead v i >>= (return . OR)
  basicUnsafeWrite (MV_OR v) i (OR x)   = GM.basicUnsafeWrite v i x
  basicClear (MV_OR v)                  = GM.basicClear v
  basicSet (MV_OR v) (OR x)             = GM.basicSet v x
  basicUnsafeCopy (MV_OR v1) (MV_OR v2) = GM.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MV_OR v) n           = MV_OR `liftM` GM.basicUnsafeGrow v n

instance GB.Vector U.Vector OR where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_OR v)         = V_OR `liftM` GB.basicUnsafeFreeze v
  basicUnsafeThaw (V_OR v)            = MV_OR `liftM` GB.basicUnsafeThaw v
  basicLength (V_OR v)                = GB.basicLength v
  basicUnsafeSlice i n (V_OR v)       = V_OR $ GB.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_OR v) i        = GB.basicUnsafeIndexM v i >>= (return . OR)
  basicUnsafeCopy (MV_OR mv) (V_OR v) = GB.basicUnsafeCopy mv v
  elemseq _ (OR x) t                  = GB.elemseq (undefined :: Vector a) x t

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
      a'   = findGamma errf zerorot ksORs
      (a'', t'') = findGammaOR errf zerorot ksOR
      fa   = toFZ Cubic a
      fa'  = toFZ Cubic a'
      fa'' = toFZ Cubic a''
      fgi  = toFZ Cubic gi
      test g1 g2 = (fromAngle $ Deg 3) > (abs $ getOmega (g1 -#- g2))
    print "=================="
    print $ "Expect: " ++ show fa
    print (test fa fa')
    print $ testGammaFit a' gms ksOR
    print $ "from " ++ show fgi ++ " got: " ++ show fa'
    print (test fa fa'')
    print $ testGammaFit a'' gms t''
    print $ "from " ++ show fgi ++ " got: " ++ show fa''

    writeUniVTKfile ("/home/edgar/Desktop/SO3ErrFunc.vtu") False vtk'

    let vtk2 = renderSO3PointsVTK (U.map quaternionToSO3 $ U.fromList [gi, toFZ Cubic gi, a, a'])
    writeUniVTKfile ("/home/edgar/Desktop/SO3ErrFuncP.vtu") False vtk2

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
  dev   = let s = G.map ((\x->x*x) . ((-) avg)) q0s in sqrt (abs $ 1 - G.sum s/n)
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
    vs = take n $ randoms gen :: [Vec3]
    q  = toQuaternion $ mkEuler (Deg 10) (Deg 15) (Deg 2)
    rs = map (\v -> toQuaternion $ mkAxisPair v (Deg 1)) vs
    qs = U.fromList $ map (q #<=) rs
  print q
  print $ averageQuaternion qs

applyDeviation :: Deg -> Quaternion -> IO Quaternion
applyDeviation w q = do
  v <- randomIO
  let d = toQuaternion $ mkAxisPair v w
  return (q #<= d)

testTesseractFitting :: IO ()
testTesseractFitting = do
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
  print $ (gt, et)
  --print $ (gf, ef)
  writeUniVTKfile ( "/home/edgar/Desktop/tess-fittest.vti" ) True (plotTesseract tt)
  writeUniVTKfile ( "/home/edgar/Desktop/tess-fittest.vtu" ) True (plotTesseractPoints ps)
