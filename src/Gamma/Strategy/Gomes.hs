{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}

module Gamma.Strategy.Gomes
       ( run ) where

import qualified Data.Vector                  as V
import qualified Data.Vector.Unboxed          as U
import qualified Data.Vector.Unboxed.Mutable  as MU
import qualified Data.HashMap.Strict          as HM
import qualified Data.HashSet                 as HS
import qualified Data.IntMap                  as IM

import           Data.Vector.Unboxed (Vector)
import           Data.HashMap.Strict (HashMap)
import           Data.HashSet        (HashSet)
import           Data.Maybe          (mapMaybe)

import           System.FilePath
import           Control.Parallel.Strategies
import           Control.Monad.RWS   (RWS(..), ask, get, put, runRWS, evalRWS)
import           Control.Monad.Trans
import           Control.Monad (liftM)

import           Hammer.Math.Algebra         (Vec3(..), Vec4(..))
import           Hammer.VoxBox
import           Hammer.VTK.VoxBox
import           Hammer.VTK
import           Hammer.Graph
import           Hammer.MicroGraph

import           Texture.Symmetry            (Symm (..), getMisoAngle)
import           Texture.IPF
import           Texture.Orientation
import           File.ANGReader

import           Gamma.Grains
import           Gamma.OR

import Debug.Trace
dbg a = trace (show a) a
dbgs s a = trace (show s ++ " <=> " ++ show a) a

data OrientationType
  = Parent  !Quaternion
  | Product !Quaternion
  deriving (Show)

data GomesConfig
  = GomesConfig
  { minGrainMisoAngle :: Deg
  , ggAngle           :: Deg
  , mgAngle           :: Deg
  , mmAngle           :: Deg
  , realOR            :: OR
  , orientationBox    :: VoxBox Quaternion
  , grainIDBox        :: VoxBox GrainID
  , grainVoxelMap     :: HashMap Int (V.Vector VoxelPos)
  , orientationMap    :: HashMap Int Quaternion
  , structureGraph    :: MicroVoxel
  }

data GomesState
  = GomesState
  { grainGroup       :: V.Vector (HashSet Int)
  , groupOrientation :: V.Vector OrientationType
  }

type Gomes = RWS GomesConfig () GomesState

-- =======================================================================================

getGomesConfig :: Deg -> Deg -> Deg -> Deg -> OR -> VoxBox Quaternion -> Maybe GomesConfig
getGomesConfig gmiso mergeGG mergeMG mergeMM ror qBox = let
  func (gidBox, voxMap) = GomesConfig
     { minGrainMisoAngle = gmiso
     , ggAngle           = mergeGG
     , mgAngle           = mergeMG
     , mmAngle           = mergeMM
     , realOR            = ror
     , orientationBox    = qBox
     , grainIDBox        = gidBox
     , grainVoxelMap     = voxMap
     , orientationMap    = avgGrainOrientation qBox voxMap
     , structureGraph    = fst $ getMicroVoxel (gidBox, voxMap)
     }
  in fmap func (getGrainID gmiso Cubic qBox)

getInitState :: GomesConfig -> GomesState
getInitState GomesConfig{..} = let
  fs    = findConnFaces orientationBox structureGraph realOR
  gids  = HM.keys $ microGrains structureGraph
  gg    = grainsGraph gids fs
  go    = getGrainGroupAvgOrientation realOR orientationMap gg
  in GomesState
     { grainGroup        = gg
     , groupOrientation  = go
     }

mergeStepSingles :: Deg -> Gomes ()
mergeStepSingles ang = do
  GomesConfig{..} <- ask
  GomesState{..}  <- get
  let
    fids  = HM.keys $ microFaces  structureGraph
    fidGG = dbgs "ggcon: " $ getGrainGroupConn fids grainGroup
  mergeableFaces <- tryMergeSingle ang fidGG
  -- Next step
  let
    gg = doMerge grainGroup mergeableFaces
    go = getGrainGroupAvgOrientation realOR orientationMap gg
  put $ GomesState
     { grainGroup       = gg
     , groupOrientation = go
     }

seeResults :: Gomes [(VTK Double, String)]
seeResults = do
  vtkIPF  <- liftM (plotMicroIPF "Alpha" . orientationBox) ask
  vtkAIPF <- liftM (plotMicroIPF "AvgAlpha") alphaQBox
  vtkID1  <- plotMicroID
  vtkIPF1 <- liftM (plotMicroIPF "Gamma") gammaQBox
  mergeStepSingles $ Deg 4
  vtkIPF2 <- liftM (plotMicroIPF "Gamma") gammaQBox
  mergeStepSingles $ Deg 8
  vtkIPF3 <- liftM (plotMicroIPF "Gamma") gammaQBox
  mergeStepSingles $ Deg 12
  vtkIPF4 <- liftM (plotMicroIPF "Gamma") gammaQBox
  return [ (vtkIPF,  "IPF-alpha")
         , (vtkAIPF, "AIPF-alpha")
         , (vtkID1,  "ID1")
         , (vtkIPF1, "IPF1")
         , (vtkIPF2, "IPF2")
         , (vtkIPF3, "IPF3")
         , (vtkIPF4, "IPF4")
         ]

run :: Deg -> FilePath -> FilePath -> IO ()
run miso fin fout = do
  ang <- parseANG fin
  let plot (vtk, name) = writeUniVTKfile (fout ++ name  <.> "vtr") True vtk
  let
    vbq = ebsdToVoxBox ang rotation
    ror = fromQuaternion $ mkQuaternion $ Vec4 7.126e-1 2.895e-1 2.238e-1 5.986e-1
    (vtks, _) = case getGomesConfig miso 4 4 4 ror vbq of
      Nothing  -> error "No grain detected!"
      Just cfg -> evalRWS seeResults cfg (getInitState cfg)
  mapM_ plot vtks

-- =======================================================================================

plotMicroID :: Gomes (VTK Double)
plotMicroID = do
  GomesConfig{..} <- ask
  GomesState{..}  <- get
  idBox <- gammaIDBox
  let
    attr1  = mkCellAttr "GammaGB" (\i _ _ ->             (grainID idBox)  U.! i)
    attr2  = mkCellAttr "AlphaGB" (\i _ _ -> unGrainID $ (grainID grainIDBox) U.! i)
  return $ renderVoxBoxVTK idBox [attr1, attr2]

gammaIDBox :: Gomes (VoxBox Int)
gammaIDBox = do
  GomesConfig{..} <- ask
  GomesState{..}  <- get
  let
    --onlyGG = V.filter ((> 1) . HS.size) gg
    boxdim            = dimension grainIDBox
    nvox              = U.length $ grainID grainIDBox
    getIS gid         = maybe (V.empty) (V.map (boxdim %@)) (HM.lookup gid grainVoxelMap)
    func v (newID, s) = mapM_ (V.mapM_ (\i -> MU.write v i newID) . getIS) (HS.toList s)
    vec = U.create $ do
      v <- MU.replicate nvox (-1)
      V.mapM_ (func v) (V.imap ((,)) grainGroup)
      return v
  return $ grainIDBox { grainID = vec }

plotMicroIPF :: String -> VoxBox Quaternion -> VTK Double
plotMicroIPF name qBox = let
  unColor (RGBColor rgb) = rgb
  getIPF = unColor . getRGBColor . snd . getIPFColor Cubic ND
  attr   = mkCellAttr name (\i _ _ -> getIPF $ (grainID qBox) U.! i)
  in renderVoxBoxVTK qBox [attr]

gammaQBox :: Gomes (VoxBox Quaternion)
gammaQBox = do
  GomesConfig{..} <- ask
  GomesState{..}  <- get
  let
    -- onlyGG = V.filter ((> 1) . HS.size) gg
    -- qVec   = getGrainGroupAvgOrientation vor qMap onlyGG
    -- qBox   = gammaQBox gidBox voxMap qVec onlyGG
    func v (newID, s) = mapM_ (V.mapM_ (\i -> MU.write v i newID) . getIS) (HS.toList s)
    foo ot s = case ot of
      Parent  q -> (q, s)
      Product q -> (q, s)
    boxdim    = dimension grainIDBox
    nvox      = U.length $ grainID grainIDBox
    getIS gid = maybe (V.empty) (V.map (boxdim %@)) (HM.lookup gid grainVoxelMap)
    vec = U.create $ do
      v <- MU.replicate nvox zerorot
      V.mapM_ (func v) (V.zipWith foo groupOrientation grainGroup)
      return v
  return $ grainIDBox { grainID = vec }

alphaQBox :: Gomes (VoxBox Quaternion)
alphaQBox = do
  GomesConfig{..} <- ask
  GomesState{..}  <- get
  let
    func v (gid, q) = V.mapM_ (\i -> MU.write v i q) $ getIS gid
    boxdim    = dimension grainIDBox
    nvox      = U.length $ grainID grainIDBox
    getIS gid = maybe (V.empty) (V.map (boxdim %@)) (HM.lookup gid grainVoxelMap)
    vec = U.create $ do
      v <- MU.replicate nvox zerorot
      mapM_ (func v) (HM.toList orientationMap)
      return v
  return $ grainIDBox { grainID = vec }

-- =======================================================================================

grainsGraph :: [GrainID] -> [FaceID] -> V.Vector (HashSet Int)
grainsGraph gids = V.fromList
                   . connComp
                   . mkUniGraph (map unGrainID gids)
                   . map ((\x -> (x, 1.0 :: Double)) . unFaceID)

findConnFaces :: VoxBox Quaternion -> MicroVoxel -> OR -> [FaceID]
findConnFaces vbq micro withOR = let
  es  = HM.keys $ microEdges micro
  ors = genTS withOR
  bs  = map (testEdge vbq micro ors) es
  fs  = bs `using` parListChunk 100 rpar
  in concatMap (\(f1, f2, f3)-> [f1, f2, f3]) (mapMaybe id fs)

testEdge :: VoxBox Quaternion -> MicroVoxel -> Vector OR ->
            EdgeID -> Maybe (FaceID, FaceID, FaceID)
testEdge vbq micro ors eid = let
  func = testFace vbq micro ors
  in case unEdgeID eid of
  Left (f1, f2, f3)
    | func f1 &&
      func f2 &&
      func f3 -> return (f1, f2, f3)
  _ -> Nothing

testFace :: VoxBox Quaternion -> MicroVoxel -> Vector OR -> FaceID -> Bool
testFace vbq micro ors fid = let
  facelist = getFaceProp fid micro >>= getPropValue
  func fs = let
    bs = V.filter id $ V.map (testSingleFace vbq ors) fs
    fn = fromIntegral $ V.length fs
    on = fromIntegral $ V.length bs
    in dbgs fid $ (on / fn) :: Double
  in maybe False ((> 0.5) . func) facelist

testSingleFace :: VoxBox Quaternion -> Vector OR -> FaceVoxelPos -> Bool
testSingleFace vbq ors face = let
  (v1, v2) = getFaceVoxels face
  q1 = vbq #! v1
  q2 = vbq #! v2
  in (fromAngle $ Deg 3) > misoOR ors Cubic q1 q2

renderGB :: VoxBox Quaternion -> MicroVoxel -> [FaceID] -> VTK Vec3
renderGB vb micro fs = addData $ renderAllElemProp vb fprop
  where
    fprop = mapMaybe ((flip getFaceProp) micro) fs
    addData vtk = let
      func _ _ _ = 1 :: Int
      in addDataCells vtk (mkCellAttr "ConnGB" func)

-- TODO move to Hammer
-- | Get both voxels that forms a given face.
getFaceVoxels :: FaceVoxelPos -> (VoxelPos, VoxelPos)
getFaceVoxels (Fx pos) = (pos, pos #+# (VoxelPos (-1) 0 0))
getFaceVoxels (Fy pos) = (pos, pos #+# (VoxelPos 0 (-1) 0))
getFaceVoxels (Fz pos) = (pos, pos #+# (VoxelPos 0 0 (-1)))

-- ================================= Re-graph ============================================

doMerge :: V.Vector (HashSet Int) -> [(Int, Int)] -> V.Vector (HashSet Int)
doMerge gg conn = let
  n = V.length gg - 1
  newgraph = grainsGraph (map mkGrainID [0 .. n]) (map mkFaceID conn)
  in V.map (HS.unions . map (gg V.!) . HS.toList) newgraph

-- | Tries to merge similar grain groups or single grain to a grain group
tryMergeSingle :: Deg -> [(Int, Int)] -> Gomes [(Int, Int)]
tryMergeSingle ang fs = do
  GomesConfig{..} <- ask
  GomesState{..}  <- get
  let
    func (g1, g2) = case (groupOrientation V.! g1, groupOrientation V.! g2) of
      (Product q1, Parent  q2) -> let
        err = errorProductParent (U.singleton q1) q2 realOR
        in dbgs "MG: " $ ang > avgError err
      (Parent  q1, Product q2) -> let
        err = errorProductParent (U.singleton q2) q1 realOR
        in dbgs "GM: " $ ang > avgError err
      _                        -> False
  return $ filter func fs

-- | Tries to merge similar grain groups or single grain to a grain group
tryMergeGroups :: Deg -> [(Int, Int)] -> Gomes [(Int, Int)]
tryMergeGroups ang fs = do
  GomesConfig{..} <- ask
  GomesState{..}  <- get
  let
    func (g1, g2) = case (groupOrientation V.! g1, groupOrientation V.! g2) of
      (Parent  q1, Parent  q2) -> dbgs "GG: " $ (fromAngle ang) > getMisoAngle Cubic q1 q2
      _                        -> False
  return $ filter func fs

-- | Calculates the parent phase for grouped grains based on their average orientation.
getGrainGroupOrientation :: OR -> HashMap Int (Vector Quaternion) -> V.Vector (HashSet Int) -> V.Vector OrientationType
getGrainGroupOrientation or0 qmap gg = let
  func = U.concat . mapMaybe ((flip HM.lookup) qmap) . HS.toList
  find qs = let
    ef = errorfunc (U.map getQinFZ qs)
    g0 = hotStartGamma ef
    in findGamma ef g0 or0
  foo s
    | HS.size s >  1 = Parent  $ find   (func s)
    | HS.size s == 1 = Product $ U.head (func s)
    | otherwise      = error "[Gomes] Empty grain group."
  in V.map foo gg

-- | Calculates the parent phase for grouped grains based on their average orientation.
getGrainGroupAvgOrientation :: OR -> HashMap Int Quaternion -> V.Vector (HashSet Int) -> V.Vector OrientationType
getGrainGroupAvgOrientation or0 qmap gg = let
  func = U.fromList . mapMaybe ((flip HM.lookup) qmap) . HS.toList
  find qs = let
    ef = errorfunc (U.map getQinFZ qs)
    g0 = hotStartGamma ef
    in findGamma ef g0 or0
  foo s
    | HS.size s >  1 = Parent  $ find   (func s)
    | HS.size s == 1 = Product $ U.head (func s)
    | otherwise      = error "[Gomes] Empty grain group."
  in V.map foo gg

-- | Map original FaceID (face between 2 grains) to faces between grouped grains.
getGrainGroupConn :: [FaceID] -> V.Vector (HashSet Int) -> [(Int, Int)]
getGrainGroupConn noconn graingroup = let
  func (i, s) = let
    oldGID = V.fromList $ HS.toList s
    gsize  = V.length oldGID
    in V.zip oldGID (V.replicate gsize i)
  m = IM.fromList $ V.toList $ V.concatMap func $ V.imap ((,)) graingroup
  foo fid = let
    (g1,g2) = unFaceID fid
    in do
      ng1 <- IM.lookup g1 m
      ng2 <- IM.lookup g2 m
      if ng1 == ng2
        then Nothing
        else return (ng1, ng2)
  in mapMaybe foo noconn

-- | Find single grains and their connections with grain groups.
getSingleGrainGroupConn :: HashMap GrainID (HashSet FaceID) -> V.Vector (HashSet Int) -> [(GrainID, [Int])]
getSingleGrainGroupConn grainconn graingroup = let
  singles = V.map (head . HS.toList) $ V.filter ((== 1) . HS.size) graingroup
  m       = IM.fromList $ V.toList $ V.concatMap func1 $ V.imap ((,)) graingroup

  func1 (i, s) = let
    oldGID = V.fromList $ HS.toList s
    gsize  = V.length oldGID
    in V.zip oldGID (V.replicate gsize i)

  func2 :: Int -> [Int] -> FaceID -> [Int]
  func2 g acc fid = let
    (g1,g2) = unFaceID  fid
    in case (IM.lookup g1 m, IM.lookup g2 m) of
      (Just ng1, Just ng2)
        | ng1 == g  -> ng2 : acc
        | ng2 == g  -> ng1 : acc
      _ -> acc

  func3 :: [(GrainID, [Int])] -> Int -> [(GrainID, [Int])]
  func3 acc g = let
    gid = mkGrainID g
    ins hs = (gid, HS.foldl' (func2 g) [] hs) : acc
    in maybe acc ins (HM.lookup gid grainconn)

  in V.foldl func3 [] singles


-- | Calculates the average orientation per grain
avgGrainOrientation :: VoxBox Quaternion ->
                       HashMap Int (V.Vector VoxelPos) ->
                       HashMap Int Quaternion
avgGrainOrientation vbq gmap = HM.map (shitQAvg . V.convert . V.map (vbq #!)) gmap
