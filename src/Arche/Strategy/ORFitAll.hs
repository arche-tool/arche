{-# LANGUAGE
    BangPatterns
  , DeriveGeneric
  , FlexibleInstances
  , RecordWildCards
  #-}

module Arche.Strategy.ORFitAll
  ( run
  , processEBSD
  , Cfg(..)
  , OrientationRelationship
    ( orValue
    , orAxis
    , orAngle
    )
  , KSDeviation
    ( directDeviation
    , planeDeviation
    , axisDeviation
    )
  , OREvaluation
    ( orientationRelationship
    , ksDeviation
    , misfitError
    )
  , evaluateOR
  ) where

import Control.Arrow       ((&&&))
import Data.Maybe          (mapMaybe)
import Data.HashMap.Strict (HashMap)
import Data.Vector.Unboxed (Vector)
import GHC.Generics        (Generic)
import System.FilePath
import System.Random.TF
import System.Random.TF.Init
import System.Random.TF.Instances
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector          as V
import qualified Data.Vector.Unboxed  as U
import qualified Data.HashMap.Strict  as HM

import File.EBSD
import Linear.Vect
import Hammer.VoxBox
import Hammer.VTK
import Hammer.MicroGraph
import Texture.Symmetry
import Texture.Orientation
import qualified File.ANGReader as A
import qualified File.CTFReader as C

import Arche.Grains
import Arche.OR

data Cfg =
  Cfg
  { misoAngle    :: Deg
  , optByAvg     :: Bool
  , predefinedOR :: Maybe AxisPair
  , startOR      :: Maybe AxisPair
  , parentPhase  :: Either Phase PhaseSymm
  , productPhase :: Phase
  } deriving (Generic, Show)

run :: Cfg -> FilePath -> FilePath -> IO ()
run cfg@Cfg{..} ebsd_file base_output = do
  bs <- BSL.readFile ebsd_file
  (orEval, vtk) <- processEBSD cfg bs
  printOREvaluation orEval 
  writeUniVTKfile (base_output <.> "vtp") True vtk

getPhaseSelector :: Cfg -> (Int -> Phase)
getPhaseSelector Cfg{..} = let
  in case parentPhase of  
    Left parent -> \ph -> if ph == phaseId parent then parent else if ph == phaseId productPhase then productPhase else Phase ph CubicPhase 
    _           -> \ph -> if ph == phaseId productPhase then productPhase else Phase ph CubicPhase 

processEBSD :: Cfg -> BSL.ByteString -> IO (OREvaluation, VTK Vec3D)
processEBSD cfg@Cfg{..} bs = do
  gen <- initTFGen
  let
    symmSelector = getPhaseSelector cfg
    parentSymm   = either getPhaseSymm getSymm parentPhase
    vbq = either error id $ do
      ebsd <- loadEBSD bs
      readEBSDToVoxBox
        (C.rotation &&& (symmSelector . C.phase))
        (A.rotation &&& (symmSelector . A.phaseNum))
        ebsd
    (gidBox, voxMap) = maybe (error "No grain detected!") id (getGrainID misoAngle vbq)
    getGoods         = U.filter ((5 >) . evalMisoORWithKS)
    mkr              = fst $ getMicroVoxel (gidBox, voxMap)
    qmap             = getGrainAverageQ vbq voxMap
    segs
      | optByAvg  = getGoods $ getGBbyAverage  qmap mkr gen 1000
      | otherwise = getGoods $ getGBbySegments vbq  mkr gen 1000

    realOR = findORFace parentSymm segs $ maybe brOR (OR . toQuaternion) startOR
    !ror = maybe realOR (OR . toQuaternion) predefinedOR
    !orEval = evaluateOR parentSymm ror segs
    vtk = renderVTK cfg vbq qmap mkr ror

  return (orEval, vtk)

renderVTK :: Cfg -> VoxBox (Quaternion, Phase) -> HashMap Int (Quaternion, Phase) -> MicroVoxel -> OR -> VTK Vec3D
renderVTK Cfg{..} vbq qmap mkr ror
  | optByAvg  = renderGBOR        parentSymm ror vbq qmap mkr
  | otherwise = renderFaceVoxelOR parentSymm ror vbq      mkr
  where
    parentSymm = either getPhaseSymm getSymm parentPhase

-- ================================== Find OR ============================================

evalMisoORWithKS :: ((Quaternion, Phase), (Quaternion, Phase)) -> Deg
evalMisoORWithKS (q1, q2) = toAngle $ evalMisoOR brORs q1 q2

getGBbySegments :: VoxBox (Quaternion, Phase) -> MicroVoxel -> TFGen
                -> Int -> Vector ((Quaternion, Phase), (Quaternion, Phase))
getGBbySegments vbq micro gen n = let
  gs = HM.elems $ microFaces micro
  fs = V.concat $ mapMaybe getPropValue gs
  rs = take n $ randomRs (0, V.length fs - 1) gen
  foo (f1, f2) = (vbq #! f1, vbq #! f2)
  in U.fromList $ map (foo . getFaceVoxels . (fs V.!)) rs

-- TODO move to Hammer
-- | Get both voxels that forms a given face.
getFaceVoxels :: FaceVoxelPos -> (VoxelPos, VoxelPos)
getFaceVoxels (Fx pos) = (pos, pos #+# (VoxelPos (-1) 0 0))
getFaceVoxels (Fy pos) = (pos, pos #+# (VoxelPos 0 (-1) 0))
getFaceVoxels (Fz pos) = (pos, pos #+# (VoxelPos 0 0 (-1)))

getGBbyAverage :: HashMap Int (Quaternion, Phase) -> MicroVoxel -> TFGen
               -> Int -> Vector ((Quaternion, Phase), (Quaternion, Phase))
getGBbyAverage qmap micro gen n = let
  fs = V.fromList $ HM.keys $ microFaces micro
  rs = take n $ randomRs (0, V.length fs - 1) gen
  foo fid = let
    (g1, g2) = unFaceID fid
    in do
      q1 <- HM.lookup g1 qmap
      q2 <- HM.lookup g2 qmap
      return (q1, q2)
  in U.fromList $ mapMaybe (foo . (fs V.!)) rs

getGrainAverageQ :: VoxBox (Quaternion, Phase)->
                    HashMap Int (V.Vector VoxelPos) ->
                    HashMap Int (Quaternion, Phase)
getGrainAverageQ vbq gmap = let
  getAvgQ gid vs = let
    q = averageQuaternion $ V.map (fst . (vbq #!)) vs
    p = maybe (Phase (-1) CubicPhase) id (getGrainPhase vbq gmap gid)
    in (q, p)
  in HM.mapWithKey getAvgQ gmap

-- ================================== Plotting ===========================================

avg :: V.Vector Double -> Double
avg x
  | n > 0     = (V.sum x) / n
  | otherwise = 0
  where
    n = fromIntegral $ V.length x

avgVector :: V.Vector Double -> V.Vector Double
avgVector x
  | n > 0     = V.replicate n (avg x)
  | otherwise = x
  where
    n = fromIntegral $ V.length x

renderFaceVoxelOR :: Symm -> OR -> VoxBox (Quaternion, Phase) -> MicroVoxel -> VTK Vec3D
renderFaceVoxelOR parentSymm ror vbq micro = let
  gs  = mapMaybe (getPropValue) $ HM.elems $ microFaces micro
  fs  = V.concat gs
  ms  = V.concat $ map (avgVector . getM) gs
  ts  = genTS parentSymm ror
  vtk = renderVoxElemListVTK vbq (V.toList fs)
  getM = V.map (((180/pi) *) . faceVoxelMisoOR ts vbq)
  func i _ _ = ms V.! i
  in addCellAttr vtk (mkCellAttr "misoORAvg" func)

renderGBOR :: Symm
           -> OR
           -> VoxBox (Quaternion, Phase)
           -> HashMap Int (Quaternion, Phase)
           -> MicroVoxel
           -> VTK Vec3D
renderGBOR parentSymm ror vbq qmap micro = let
  fids = HM.keys  $ microFaces micro
  vs   = HM.elems $ microFaces micro
  fs   = mapMaybe getPropValue vs
  ts   = genTS parentSymm ror
  ms   = V.concat $ zipWith foo fids fs
  vtk  = renderVoxElemListVTK vbq (concatMap V.toList fs)
  foo fid vf = V.replicate (V.length vf) ((180/pi) * faceMisoOR ts qmap fid)
  func i _ _ = ms V.! i
  in addCellAttr vtk (mkCellAttr "misoOR" func)

faceVoxelMisoOR :: U.Vector OR -> VoxBox (Quaternion, Phase) -> FaceVoxelPos -> Double
faceVoxelMisoOR ors vbq face = let
  (v1, v2) = getFaceVoxels face
  q1 = vbq #! v1
  q2 = vbq #! v2
  in evalMisoOR ors q1 q2

faceMisoOR :: U.Vector OR -> HashMap Int (Quaternion, Phase) -> FaceID -> Double
faceMisoOR ors qmap face = maybe pi id getM
  where
    (v1, v2) = unFaceID face
    getM = do
      q1 <- HM.lookup v1 qmap
      q2 <- HM.lookup v2 qmap
      return $ evalMisoOR ors q1 q2

-- ================================== Results ============================================

--ks = OR $ toQuaternion $ mkAxisPair (Vec3 2 2 11) (Deg 42.85)
--nw = OR $ toQuaternion $ mkAxisPair (Vec3 2 5 24) (Deg 45.98)
--gt = OR $ toQuaternion $ mkAxisPair (Vec3 2 3 15) (Deg 44.23)

data OrientationRelationship
  = OrientationRelationship
  { orValue :: !OR
  , orAxis :: !(Int, Int, Int)
  , orAngle :: !Deg
  } deriving (Generic, Show)

data KSDeviation
  = KSDeviation
  { directDeviation :: !Deg
  , planeDeviation :: !Deg
  , axisDeviation :: !Deg
  } deriving (Generic, Show)

data OREvaluation
  = OREvaluation
  { orientationRelationship :: !OrientationRelationship
  , ksDeviation :: !KSDeviation
  , misfitError :: !FitError
  } deriving (Generic, Show)

evaluateOR :: Symm -> OR -> Vector ((Quaternion, Phase), (Quaternion, Phase)) -> OREvaluation
evaluateOR parentSymm ror segs = OREvaluation
  { orientationRelationship = mkOrientationRelationship ror
  , ksDeviation = calculateKSDeviation parentSymm ror
  , misfitError = faceerrorfunc segs (genTS parentSymm ror)
  }

mkOrientationRelationship :: OR -> OrientationRelationship
mkOrientationRelationship ror = let
  ap    = fromQuaternion (qOR ror)
  (v,w) = axisAngle ap
  axis  = aproxToIdealAxis v 0.001
  in OrientationRelationship
    { orValue = ror
    , orAxis = axis
    , orAngle = toAngle w
    }

calculateKSDeviation :: Symm -> OR -> KSDeviation
calculateKSDeviation parentSymm ror = let
  rors = genTS parentSymm ror

  evalManyOR :: (OR -> Double) -> Deg
  evalManyOR func = toAngle $ U.minimum $ U.map func rors

  evalVecRot v1 v2 = let
    allVec = getAllSymmVec (getSymmOps Cubic) v2
    in U.minimum . (\v -> U.map (angle v) allVec) . passiveVecRotation v1 . qOR

  devPlane = evalVecRot (Vec3 1 1 0) (Vec3 1 1 1)
  devDir   = evalVecRot (Vec3 1 1 1) (Vec3 1 1 0)
  dev      = getMisoAngle Cubic (qOR brOR) . qOR
  in KSDeviation
    { directDeviation = evalManyOR dev
    , planeDeviation = evalManyOR devPlane
    , axisDeviation = evalManyOR devDir
    }

printKSDev :: KSDeviation -> IO ()
printKSDev KSDeviation{..} = do
  putStrLn $ "Direct deviation from KS: "       ++ show directDeviation
  putStrLn $ "Deviation from (111) <-> (110): " ++ show planeDeviation
  putStrLn $ "Deviation from [110] <-> [111]: " ++ show axisDeviation

printOR :: OrientationRelationship -> IO ()
printOR OrientationRelationship{..} = let
  (h,k,l) = orAxis
  msg = "OR: " ++ show [h, k, l] ++ " " ++ show orAngle
  in putStrLn msg

printOREvaluation :: OREvaluation -> IO ()
printOREvaluation OREvaluation{..} = do
  putStrLn $ "Error: " ++ show misfitError
  printOR orientationRelationship
  printKSDev ksDeviation