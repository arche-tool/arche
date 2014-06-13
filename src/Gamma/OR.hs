{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Gamma.OR
       ( findGamma
       , findGammaOR
       , findOR
       , findORFace
       , hotStartGamma
       , hotStartOR
       , OR (..)
       , FitError (avgError, devError, maxError)
       , convert
       , mkOR
       , genTS
       , getQinFZ
       , ksORs
       , ksOR
       , misoOR
       , misoKS
       , errorProductParent
       , wErrorProductParent
       , weightederrorfunc
       , errorfunc
         -- * Test functions
       , testGammaFit
       , testFindGamma
       , testMisFunc
       , testMisoKS
       , shitQAvg
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

--dbg a = trace (show a) a

-- ======================================================================================= 

newtype FZ = FZ {qFZ :: Quaternion} deriving (Show)
newtype OR = OR {qOR :: Quaternion} deriving (Show)

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

getQinFZ :: Quaternion -> FZ
getQinFZ = FZ . toFZ Cubic

ksOR :: OR
ksOR = OR $ toQuaternion $ mkAxisPair (Vec3 1 1 2) (Deg 90)

ksORs :: Vector OR
ksORs = genTS ksOR

genTS :: OR -> Vector OR
genTS (OR t) = let
  (w, v) = splitQuaternion t
  vs = V.convert $ getAllSymmVec (getSymmOps Cubic) v
  in G.map (OR . mergeQuaternion . ((,) w)) vs

misoKS :: Symm -> Quaternion -> Quaternion -> Double
misoKS = misoOR ksORs

misoOR :: Vector OR -> Symm -> Quaternion -> Quaternion -> Double
misoOR ors symm q1 q2 = let
  ks1 = U.map ((q1 #<=) . qOR) ors
  ks2 = U.map ((q2 #<=) . qOR) ors
  -- Fully correct. Need prove that works!
  foo q = U.map (getMisoAngle symm q) ks2
  in U.minimum $ U.concatMap foo ks1

data FitError
  = FitError
  { avgError :: Deg
  , devError :: Deg
  , maxError :: Deg
  } deriving (Show)

type ErrorFunc = Quaternion -> Vector OR -> FitError

-- | Find the angular error in rad between product and parent orientations regarding a
-- given orientation relationship.
wErrorProductParent :: Vector Double -> Vector Quaternion -> Quaternion -> OR -> FitError
wErrorProductParent ws qs ga t0 = let
  fzqs = G.map getQinFZ qs
  in weightederrorfunc ws fzqs ga (genTS t0)

-- | Find the angular error in rad between product and parent orientations regarding a
-- given orientation relationship.
errorProductParent :: Vector Quaternion -> Quaternion -> OR -> FitError
errorProductParent qs ga t0 = let
  fzqs = G.map getQinFZ qs
  in errorfunc fzqs ga (genTS t0)

findORFace :: Vector (Quaternion, Quaternion) -> OR -> OR
findORFace qs t0 = let
  err t = let
    n    = U.length qs
    errs = U.map (\(q1, q2) -> misoOR (genTS t) Cubic q1 q2) qs
    in U.sum errs / (fromIntegral n)
  func = err . OR . toQuaternion . mkUnsafeRodrigues
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

findGamma :: ErrorFunc -> Quaternion -> OR -> Quaternion
findGamma errf q0 or0 = let
  func v = let
    gamma = toQuaternion $ mkUnsafeRodrigues v
    in fromAngle $ avgError $ errf gamma (genTS or0)
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

-- | Evaluates the average angular error in rad between given parent and product
-- orientations and given orientation relationship. The list of products is given in the
-- fundamental zone.
errorfunc :: Vector FZ -> Quaternion-> Vector OR -> FitError
errorfunc gms1FZ ga ts = let
  toAng = toAngle . (2 *) . acosSafe
  func gm1 gm2 = abs $ composeQ0 (invert gm2) (qFZ gm1)
  gms2  = G.map (toFZ Cubic . (ga #<=) . qOR) ts
  q0s   = G.map (\gm1 -> G.maximum $ G.map (func gm1) gms2) gms1FZ
  n     = fromIntegral (G.length gms1FZ)
  dev   = let s = G.map ((\x->x*x) . ((-) avg)) q0s in sqrt (abs $ 1 - G.sum s / n)
  avg   = G.sum q0s / n
  in FitError
     { avgError = toAng avg
     , devError = toAng dev
     , maxError = toAng (G.minimum q0s)
     }

-- | Evaluates the average angular error in rad between given parent and product
-- orientations and given orientation relationship. The list of products is given in the
-- fundamental zone.
weightederrorfunc :: Vector Double -> Vector FZ -> Quaternion-> Vector OR -> FitError
weightederrorfunc ws gms1FZ ga ts = let
  func gm1 gm2 = abs $ composeQ0 (invert gm2) (qFZ gm1)
  toRad = abs . (2 *) . acosSafe
  gms2  = G.map (toFZ Cubic . (ga #<=) . qOR) ts
  q0s   = G.map (\gm1 -> toRad . G.maximum $ G.map (func gm1) gms2) gms1FZ
  wt    = G.sum ws
  wq    = G.zipWith (*) ws q0s
  dev   = let
    s = G.map ((\x->x*x) . ((-) avg)) q0s
    in sqrt (G.sum (G.zipWith (*) ws s) / wt)
  avg   = G.sum wq / wt
  in FitError
     { avgError = toAngle avg
     , devError = toAngle dev
     , maxError = toAngle (G.maximum wq / wt)
     }

-- =========================================== Unbox FZ  =================================

newtype instance U.MVector s FZ = MV_FZ (U.MVector s Quaternion)
newtype instance U.Vector    FZ = V_FZ  (U.Vector    Quaternion)

instance U.Unbox FZ

instance GM.MVector U.MVector FZ where
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
  basicLength (MV_FZ v)                 = GM.basicLength v
  basicUnsafeSlice i n (MV_FZ v)        = MV_FZ $ GM.basicUnsafeSlice i n v
  basicOverlaps (MV_FZ v1) (MV_FZ v2)   = GM.basicOverlaps v1 v2
  basicUnsafeNew n                      = MV_FZ `liftM` GM.basicUnsafeNew n
  basicUnsafeReplicate n (FZ x)         = MV_FZ `liftM` GM.basicUnsafeReplicate n x
  basicUnsafeRead (MV_FZ v) i           = GM.basicUnsafeRead v i >>= (return . FZ)
  basicUnsafeWrite (MV_FZ v) i (FZ x)   = GM.basicUnsafeWrite v i x
  basicClear (MV_FZ v)                  = GM.basicClear v
  basicSet (MV_FZ v) (FZ x)             = GM.basicSet v x
  basicUnsafeCopy (MV_FZ v1) (MV_FZ v2) = GM.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MV_FZ v) n           = MV_FZ `liftM` GM.basicUnsafeGrow v n

instance GB.Vector U.Vector FZ where
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
  basicUnsafeIndexM (V_FZ v) i        = GB.basicUnsafeIndexM v i >>= (return . FZ)
  basicUnsafeCopy (MV_FZ mv) (V_FZ v) = GB.basicUnsafeCopy mv v
  elemseq _ (FZ x) t                  = GB.elemseq (undefined :: Vector a) x t

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
  m1  = errorfunc gms ga (genTS t)
  m2  = errorfuncSlowButSure ga gs (qOR t)
  in (m1, m2)

testMisFunc :: IO ()
testMisFunc = do
  a <- randomIO
  let gms = G.map (getQinFZ . (a #<=) . toQuaternion) ksORs
  print $ errorfuncSlowButSure a (G.map qFZ gms) (qOR ksOR)
  print $ errorfunc gms a ksORs

testFindOR :: IO ()
testFindOR = do
  a <- randomIO
  let
    t  = mkOR (Vec3 1 1 2) (Deg 90)
    ts = genTS t
    ms = G.map ((a #<=) . qOR) ts
    t' = findOR errf a t
    errf = errorfunc fzqs
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
  errf = errorfunc fzqs
  (grid, vtk) = mkSO3 35 35 35
  es = G.map (\s -> fromAngle $ avgError $ errorfunc fzqs (so3ToQuaternion s) ksORs) grid
  attr = mkPointAttr "Error function" (es U.!)
  vtk' = addPointAttr vtk attr

  gi   = so3ToQuaternion $ grid U.! (U.minIndex es)
  in do
    let
      a'   = findGamma errf zerorot ksOR
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
