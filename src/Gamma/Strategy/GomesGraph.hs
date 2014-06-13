{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}

module Gamma.Strategy.GomesGraph
       ( run ) where

import qualified Data.Vector                  as V
import qualified Data.Vector.Unboxed          as U
import qualified Data.Vector.Unboxed.Mutable  as MU
import qualified Data.HashMap.Strict          as HM
import qualified Data.List                    as L

import           Data.Vector.Unboxed (Vector)
import           Data.HashMap.Strict (HashMap)
import           Data.Maybe          (mapMaybe)
import           Control.Applicative ((<$>))
import           Control.Monad.ST    (runST)

import           System.FilePath
import           System.IO
import           System.Process
import           Control.Parallel.Strategies
import           Control.Monad.RWS   (RWST(..), ask, get, put, runRWST)
import           Control.Monad.Trans
import           Control.Monad (liftM, zipWithM_)

import           Hammer.Math.Algebra
import           Hammer.VoxBox
import           Hammer.VTK.VoxBox
import           Hammer.VTK
import           Hammer.Graph
import           Hammer.MicroGraph

import           Texture.Symmetry            (Symm (..), getMisoAngle, toFZ)
import           Texture.IPF
import           Texture.Orientation
import           File.ANGReader

import           Gamma.Grains
import           Gamma.OR
import           Gamma.OMRender

import Debug.Trace
dbg a = trace (show a) a
dbgs s a = trace (show s ++ " <=> " ++ show a) a

data OrientationType
  = Parent  !Quaternion
  | Product !Quaternion
  deriving (Show)

data GomesConfig
  = GomesConfig
  { outputDir         :: FilePath
  , minGrainMisoAngle :: Deg
  , ggAngle           :: Deg
  , mgAngle           :: Deg
  , mmAngle           :: Deg
  , realOR            :: OR
  , orientationBox    :: VoxBox Quaternion
  , grainIDBox        :: VoxBox GrainID
  , grainVoxelMap     :: HashMap Int (V.Vector VoxelPos)
  , orientationMap    :: HashMap Int Quaternion
  , positionMap       :: HashMap Int Vec3
  , structureGraph    :: MicroVoxel
  }

data GomesState
  = GomesState
  { grainGraph  :: Graph Int Double
  , grainGroups :: [[Int]]
  , voxelGraph  :: Graph Int Double
  , voxelGroups :: [[Int]]
  }

type Gomes = RWST GomesConfig () GomesState IO

-- =======================================================================================

getGomesConfig ::FilePath -> Deg -> Deg -> Deg -> Deg -> OR -> VoxBox Quaternion -> Maybe GomesConfig
getGomesConfig dirout gmiso mergeGG mergeMG mergeMM ror qBox = let
  func (gidBox, voxMap) = GomesConfig
     { outputDir         = dirout
     , minGrainMisoAngle = gmiso
     , ggAngle           = mergeGG
     , mgAngle           = mergeMG
     , mmAngle           = mergeMM
     , realOR            = ror
     , orientationBox    = qBox
     , grainIDBox        = gidBox
     , grainVoxelMap     = voxMap
     , orientationMap    = avgGrainOrientation qBox voxMap
     , positionMap       = avgGrainPos         qBox voxMap
     , structureGraph    = fst $ getMicroVoxel (gidBox, voxMap)
     }
  in fmap func (getGrainID gmiso Cubic qBox)

getInitState :: GomesConfig -> GomesState
getInitState GomesConfig{..} = let
  --g = graphMisOR orientationBox structureGraph realOR
  gg = graphWeight orientationBox structureGraph realOR
  vg = graphWeightVBox orientationBox realOR
  in GomesState
     { grainGraph  = gg
     , grainGroups = []
     , voxelGraph  = vg
     , voxelGroups = []
     }

plotVTK_D :: (RenderElemVTK a)=> String -> (VTK a, FilePath) -> IO ()
plotVTK_D dirout (vtk, name) = writeUniVTKfile (dirout ++ name  <.> "vtr") True vtk

plotVTK_V :: (RenderElemVTK a)=> String -> (VTK a, FilePath) -> IO ()
plotVTK_V dirout (vtk, name) = writeUniVTKfile (dirout ++ name  <.> "vtu") True vtk

grainClustering :: Gomes ()
grainClustering = do
  GomesConfig{..} <- ask
  -- run MCL
  st <- get
  gg <- liftIO $ do
    saveGraph (grainGraph st)
    runMCL
    readGroups
  put $ st { grainGroups = gg }

  (vbq, attrsQGrain) <- gammaQGrain
  vbid               <- gammaIDGrain
  let
    attrGID  = plotMicroID  "gammaID"  vbid
    attrGIPF = plotMicroIPF "gammaIPF" vbq
    vtk      = renderVoxBoxVTK vbq (attrGID:attrGIPF:attrsQGrain)
  liftIO $ plotVTK_D outputDir (vtk,  "grain-gamma")

plotInput :: Gomes ()
plotInput = do
  GomesConfig{..} <- ask
  alphaQBox <- getAlphaQBox
  vtkGGraph <- plotGrainGraph
  let
    attrIPF  = plotMicroIPF "VoxelAlpha" orientationBox
    attrAIPF = plotMicroIPF "AvgAlpha"   alphaQBox
    vtk      = renderVoxBoxVTK orientationBox (attrIPF : [attrAIPF])
  liftIO $ plotVTK_D outputDir (vtk,  "IPF-alpha")
  liftIO $ plotVTK_V outputDir (vtkGGraph,  "GrainGraph")

run :: Deg -> FilePath -> FilePath -> IO ()
run miso fin fout = do
  ang <- parseANG fin
  let
    vbq = ebsdToVoxBox ang rotation
    ror = fromQuaternion $ mkQuaternion $ Vec4 7.126e-1 2.895e-1 2.238e-1 5.986e-1
    doit = do
      plotInput
      grainClustering
      --voxelClustering
      --plotGroupSO3
  _ <- case getGomesConfig fout miso 4 4 4 ror vbq of
    Nothing  -> error "No grain detected!"
    Just cfg -> runRWST doit cfg (getInitState cfg)
  return ()

-- ====================================== Plotting =======================================

plotMicroID :: (RenderElemVTK a)=> String -> VoxBox a -> VTKAttrPoint Double
plotMicroID name iBox = mkPointAttr name ((grainID iBox) U.!)

plotMicroIPF :: String -> VoxBox Quaternion -> VTKAttrPoint Double
plotMicroIPF name qBox = let
  unColor (RGBColor rgb) = rgb
  getIPF = unColor . getRGBColor . snd . getIPFColor Cubic ND
  in mkPointAttr name (getIPF . ((grainID qBox) U.!))

getAlphaQBox :: Gomes (VoxBox Quaternion)
getAlphaQBox = do
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

filterIsleGrains :: Graph Int Double -> Graph Int Double
filterIsleGrains Graph{..} = let
  func acc k v
    | HM.size v /= 1 = acc
    | otherwise      = let [t] = HM.keys v in (k, t) : acc
  foo acc (k, t) = HM.adjust (HM.delete k) t acc

  singles = HM.foldlWithKey' func [] graph
  clean1  = HM.filter ((/= 1) . HM.size) graph
  clean2  = L.foldl' foo clean1 singles
  in Graph clean2

graphMisOR :: VoxBox Quaternion -> MicroVoxel -> OR -> Graph Int Double
graphMisOR vbq micro withOR = let
  fs    = HM.keys $ microFaces micro
  ors   = genTS withOR
  ms    = map (getFaceIDmisOR vbq micro ors) fs
  mspar = ms `using` parListChunk 100 rpar
  in mkUniGraph [] $ zipWith (\fid x -> (unFaceID fid, maybe (-1) abs x)) fs mspar

getFaceIDmisOR :: VoxBox Quaternion -> MicroVoxel -> Vector OR -> FaceID -> Maybe Double
getFaceIDmisOR vbq micro ors fid = let
  facelist = getFaceProp fid micro >>= getPropValue
  func fs = let
    t = V.sum $ V.map (getFaceVoxelmisOR vbq ors) fs
    n = fromIntegral $ V.length fs
    in dbgs fid $ (t / n) :: Double
  in func <$> facelist

getFaceVoxelmisOR :: VoxBox Quaternion -> Vector OR -> FaceVoxelPos -> Double
getFaceVoxelmisOR vbq ors face = let
  (v1, v2) = getFaceVoxels face
  q1 = vbq #! v1
  q2 = vbq #! v2
  in misoOR ors Cubic q1 q2

-- TODO move to Hammer
-- | Get both voxels that forms a given face.
getFaceVoxels :: FaceVoxelPos -> (VoxelPos, VoxelPos)
getFaceVoxels (Fx pos) = (pos, pos #+# (VoxelPos (-1) 0 0))
getFaceVoxels (Fy pos) = (pos, pos #+# (VoxelPos 0 (-1) 0))
getFaceVoxels (Fz pos) = (pos, pos #+# (VoxelPos 0 0 (-1)))

-- | Calculates the average orientation per grain
avgGrainOrientation :: VoxBox Quaternion ->
                       HashMap Int (V.Vector VoxelPos) ->
                       HashMap Int Quaternion
avgGrainOrientation vbq gmap = HM.map (shitQAvg . V.convert . V.map (vbq #!)) gmap

avgGrainPos :: VoxBox Quaternion ->
               HashMap Int (V.Vector VoxelPos) ->
               HashMap Int Vec3
avgGrainPos vb gmap = HM.map func gmap
  where
    func v = let
      t = V.foldl' (&+) zero $ V.map (evalCentralVoxelPos vb) v
      n = V.length v
      in t &* (1 / fromIntegral n)

-- ================================== Grain clustering ===================================

getFaces :: Vector OR -> VoxBox Quaternion -> [((Int, Int), Double)]
getFaces ors VoxBox{..} = V.foldl' go [] (V.fromList $ getRangePos dimension)
  where
    go acc pos = let
      v    = pos
      vx   = pos #+# (VoxelPos (-1)   0    0 )
      vy   = pos #+# (VoxelPos   0  (-1)   0 )
      vz   = pos #+# (VoxelPos   0    0  (-1))
      i    = dimension %@ v
      ix   = dimension %@? vx
      iy   = dimension %@? vy
      iz   = dimension %@? vz

      isInGrain a b = let
        omega = getMisoAngle Cubic a b
        in (abs $ fromAngle $ Deg 5) > omega

      getValue (Just j) = let
        qa  = grainID U.! i
        qb  = grainID U.! j
        mOR = misoOR ors Cubic qa qb
        x   = if isInGrain qa qb then 0 else mOR
        in Just ((i, j), x)
      getValue _ = Nothing

      fs = mapMaybe getValue [ix, iy, iz]
      in fs ++ acc

graphWeight :: VoxBox Quaternion -> MicroVoxel -> OR -> Graph Int Double
graphWeight vbq micro withOR = let
  fs    = HM.keys $ microFaces micro
  ors   = genTS withOR
  ms    = map (getFaceIDmisOR vbq micro ors) fs
  ams
    | n > 0     = s / (fromIntegral n)
    | otherwise = 0
    where
      n = length ms
      s = L.foldl' (\acc x -> maybe acc (acc +) x) 0 ms
  weight x = let
    k = -300
    w = 10 * exp (k * x * x)
    in w -- if w < 0.05 then 0 else 1
  mspar = ms `using` parListChunk 100 rpar
  in filterIsleGrains $
     mkUniGraph [] $
     filter ((>= 0) . snd) $
     zipWith (\fid x -> (unFaceID fid, maybe (-1) weight x)) fs mspar

cluster :: Gomes ()
cluster = do
  st@GomesState{..} <- get
  let g = applyNSOperator 3 grainGraph
  put $ st { grainGraph = g }

plotGrainGraph :: Gomes (VTK Vec3)
plotGrainGraph = do
  GomesConfig{..} <- ask
  GomesState{..}  <- get
  let
    maxi = maximum $ HM.keys positionMap
    ns   = U.replicate (maxi+1) zero U.// (HM.toList positionMap)
    (es, vs) = unzip $ graphToList grainGraph
    vtk = mkUGVTK "graph" ns es [] [attr]
    vv  = V.fromList vs
    attr = mkCellAttr "misoOR" (\i _ _ -> vv V.! i)
  return vtk

gammaIDGrain :: Gomes (VoxBox Int)
gammaIDGrain = do
  GomesConfig{..} <- ask
  GomesState{..}  <- get
  let
    boxdim    = dimension grainIDBox
    nvox      = U.length $ grainID grainIDBox
    getIS gid = maybe (V.empty) (V.map (boxdim %@)) (HM.lookup gid grainVoxelMap)
    func v (newGID, gs) = mapM_ (\gid -> V.mapM_ (\i -> MU.write v i newGID) $ getIS gid) gs
    vec = U.create $ do
      v <- MU.replicate nvox (-1)
      mapM_ (func v) (zip [1..] grainGroups)
      return v
  return $ grainIDBox { grainID = vec }

gammaQGrain :: Gomes (VoxBox Quaternion, [VTKAttrPoint Double])
gammaQGrain = do
  GomesConfig{..} <- ask
  GomesState{..}  <- get
  let
    boxdim    = dimension grainIDBox
    nvox      = U.length $ grainID grainIDBox
    getIS gid = maybe (V.empty) (V.map (boxdim %@)) (HM.lookup gid grainVoxelMap)
    getQ  gid = maybe zerorot id (HM.lookup gid orientationMap)
    func q e d m k gs = let
      (gamma, _) = getWGammaOR ws qs
      is  = map (V.convert . getIS) gs
      ws  = U.fromList (map (fromIntegral . U.length) is)
      qs  = U.map getQ (U.fromList gs)
      iv  = U.concat is
      err = wErrorProductParent ws qs gamma realOR
      foo (qi, wi) vids = let
        gerr = wErrorProductParent (U.singleton wi) (U.singleton qi) gamma realOR
        in U.mapM_ (\i -> MU.write k i (unDeg $ avgError gerr)) vids
      in do
        U.mapM_ (\i -> MU.write q i gamma) iv
        U.mapM_ (\i -> MU.write e i (unDeg $ avgError err)) iv
        U.mapM_ (\i -> MU.write d i (unDeg $ devError err)) iv
        U.mapM_ (\i -> MU.write m i (unDeg $ maxError err)) iv
        zipWithM_ foo (U.toList $ U.zip qs ws) is
    (vq, ve, vd, vm, vk) = runST $ do
      q <- MU.replicate nvox zerorot
      e <- MU.replicate nvox (-1)
      d <- MU.replicate nvox (-1)
      m <- MU.replicate nvox (-1)
      k <- MU.replicate nvox (-1)
      mapM_ (func q e d m k) grainGroups
      q' <- U.unsafeFreeze q
      e' <- U.unsafeFreeze e
      d' <- U.unsafeFreeze d
      m' <- U.unsafeFreeze m
      k' <- U.unsafeFreeze k
      return (q', e', d', m', k')
  return ( grainIDBox { grainID = vq }
         , [ mkPointAttr "avgError[deg]"   (ve U.!)
           , mkPointAttr "devError[deg]"   (vd U.!)
           , mkPointAttr "maxError[deg]"   (vm U.!)
           , mkPointAttr "grainError[deg]" (vk U.!)
           ]
         )

plotGroupSO3 :: Gomes ()
plotGroupSO3 = do
  GomesConfig{..} <- ask
  GomesState{..}  <- get
  let
    foo vtk name gid = writeUniVTKfile ( outputDir ++ "gid_" ++
                                         show gid  ++ name <.> "vtu" ) True vtk
    func gid mids = do
      (gamma, qs, _)        <- fitGroupGrains mids
      (vtk_m, vtk_g, vtk_a) <- plotFitSO3 gamma mids qs
      liftIO (foo vtk_m "-SO3-alpha"      gid)
      liftIO (foo vtk_g "-SO3-gamma"      gid)
      liftIO (foo vtk_a "-SO3-simu_alpha" gid)
  zipWithM_ func [1..] grainGroups

-- TODO DRY!!! with func get getQGrain2
fitGroupGrains :: [Int] -> Gomes (Quaternion, Vector Quaternion, FitError)
fitGroupGrains mids = do
  GomesConfig{..} <- ask
  GomesState{..}  <- get
  let
    boxdim    = dimension grainIDBox
    getIS gid = maybe (V.empty) (V.map (boxdim %@)) (HM.lookup gid grainVoxelMap)
    getQ  gid = maybe zerorot id (HM.lookup gid orientationMap)
    func gs = let
      (gamma, _) = getWGammaOR ws qs
      is  = map (V.convert . getIS) gs
      ws  = U.fromList (map (fromIntegral . U.length) is)
      qs  = U.map getQ (U.fromList gs)
      err = wErrorProductParent ws qs gamma realOR
      in (gamma, qs, err)
  return (func mids)

plotFitSO3 :: Quaternion -> [Int] -> Vector Quaternion -> Gomes (VTK Vec3, VTK Vec3, VTK Vec3)
plotFitSO3 gamma mids ms = do
  GomesConfig{..} <- ask
  GomesState{..}  <- get
  let
    ggid = U.singleton (mkGrainID $ -5)
    as   = U.map (toFZ Cubic . (gamma #<=) . qOR) (genTS realOR)
    agid = U.replicate (U.length as) (mkGrainID $ -10)
    mgid = U.map mkGrainID $ U.fromList mids
    vtkSO3_m = renderSO3Points Cubic ND mgid ms
    vtkSO3_g = renderSO3Points Cubic ND ggid (U.singleton gamma)
    vtkSO3_a = renderSO3Points Cubic ND agid as
  return (vtkSO3_m, vtkSO3_g, vtkSO3_a)

getGammaOR :: Vector Quaternion -> (Quaternion, OR)
getGammaOR qs = let
  ef = errorfunc (U.map getQinFZ qs)
  g0 = hotStartGamma ef
  in findGammaOR ef g0 ksOR

getWGammaOR :: Vector Double -> Vector Quaternion -> (Quaternion, OR)
getWGammaOR ws qs = let
  ef = weightederrorfunc ws (U.map getQinFZ qs)
  g0 = hotStartGamma ef
  in findGammaOR ef g0 ksOR

-- ========================================= MCL =========================================

saveGraph :: (Show a, Show b)=> Graph a b -> IO ()
saveGraph g = let
  xs = graphToList g
  func f ((n1,n2), x) = hPutStrLn f (unwords $ [show n1, show n2, show x])
  in do
    f <- openFile "test.txt" WriteMode
    mapM_ (func f) xs
    hClose f

runMCL :: IO ()
runMCL = callCommand "mcl test.txt --abc -I 1.2 -o test.out"

readGroups :: IO ([[Int]])
readGroups = readFile "test.out" >>= return . map (map read . words) . lines
