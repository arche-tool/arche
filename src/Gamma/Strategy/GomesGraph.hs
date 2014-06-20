{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}

module Gamma.Strategy.GomesGraph
       ( run ) where

import qualified Data.Vector                  as V
import qualified Data.Vector.Unboxed          as U
import qualified Data.Vector.Unboxed.Mutable  as MU
import qualified Data.HashMap.Strict          as HM
import qualified Data.HashSet                 as HS
import qualified Data.List                    as L

import           Control.Applicative ((<$>))
import           Data.Vector.Unboxed (Vector)
import           Data.HashMap.Strict (HashMap)
import           Data.Word           (Word8)

import           System.FilePath
import           System.IO
import           System.Process
import           Control.Parallel.Strategies
import           Control.Monad.RWS   (RWST(..), ask, get, put, runRWST)
import           Control.Monad.Trans
import           Control.Monad (zipWithM_)

import           Hammer.Math.Algebra
import           Hammer.VoxBox
import           Hammer.VTK.VoxBox
import           Hammer.VTK
import           Hammer.Graph
import           Hammer.MicroGraph

import           Texture.Symmetry            (Symm (..), toFZ)
import           Texture.IPF
import           Texture.Orientation
import           Texture.TesseractGrid
import           File.ANGReader

import           Gamma.Grains
import           Gamma.OR
import           Gamma.OMRender

import Debug.Trace
dbg a = trace (show a) a
dbgs s a = trace (show s ++ " <=> " ++ show a) a

data ProductGrain =
  ProductGrain
  { productVoxelPos       :: V.Vector Int
  , productAvgOrientation :: QuaternionFZ
  , productAvgPos         :: Vec3
  } deriving (Show)

data ParentGrain =
  ParentGrain
  { productMembers        :: [Int]
  , parentOrientation     :: Quaternion
  , parentAvgErrorFit     :: FitError
  , parentErrorPerProduct :: [(Double, Deg)]
  } deriving (Show)

data GomesConfig
  = GomesConfig
  { outputDir         :: FilePath
  , minGrainMisoAngle :: Deg
  , ggAngle           :: Deg
  , mgAngle           :: Deg
  , mmAngle           :: Deg
  , realOR            :: OR
  , realORs           :: Vector OR
  , orientationBox    :: VoxBox Quaternion
  , grainIDBox        :: VoxBox GrainID
  , structureGraph    :: MicroVoxel
  , productGrains     :: HashMap Int ProductGrain
  , productGraph      :: Graph Int Double
  }

data GomesState
  = GomesState
  { parentGrains :: V.Vector ParentGrain
  } deriving (Show)

type Gomes = RWST GomesConfig () GomesState IO

-- =======================================================================================

getGomesConfig ::FilePath -> Deg -> Deg -> Deg -> Deg -> OR -> VoxBox Quaternion -> Maybe GomesConfig
getGomesConfig dirout gmiso mergeGG mergeMG mergeMM ror qBox = let
  func (gidBox, voxMap) = let
    micro = fst $ getMicroVoxel (gidBox, voxMap)
    in GomesConfig
     { outputDir         = dirout
     , minGrainMisoAngle = gmiso
     , ggAngle           = mergeGG
     , mgAngle           = mergeMG
     , mmAngle           = mergeMM
     , realOR            = ror
     , realORs           = genTS ror
     , orientationBox    = qBox
     , grainIDBox        = gidBox
     , productGrains     = calcProductGrains qBox voxMap
     , structureGraph    = micro
     , productGraph      = graphWeight qBox micro ror
     }
  in fmap func (getGrainID gmiso Cubic qBox)

getInitState :: GomesConfig -> GomesState
getInitState GomesConfig{..} = let
  in GomesState { parentGrains = V.empty }

grainClustering :: Gomes ()
grainClustering = do
  cfg@GomesConfig{..} <- ask
  -- run MCL
  st <- get
  gg <- liftIO $ findClusters productGraph 1.2
  let ps = V.map (getParentGrainData cfg) gg
  put $ st { parentGrains = ps }

plotResults :: String -> Gomes ()
plotResults name = do
  GomesConfig{..} <- ask
  attrGID    <- genParentVTKAttr (-1) fst "ParentGrainID"
  attrIPF    <- genParentVTKAttr (255,255,255) (getCubicIPFColor . parentOrientation . snd) "GammaIPF"
  attrAvgErr <- genParentVTKAttr (-1) (unDeg . avgError . parentAvgErrorFit . snd) "AvgError"
  attrDevErr <- genParentVTKAttr (-1) (unDeg . devError . parentAvgErrorFit . snd) "DevError"
  attrMaxErr <- genParentVTKAttr (-1) (unDeg . maxError . parentAvgErrorFit . snd) "MaxError"

  attrMID    <- genLocalErrorVTKAttr (-1) fst           "ProductGrainAreaFraction"
  attrLocErr <- genLocalErrorVTKAttr (-1) (unDeg . snd) "ErrorPerProduct"

  attrAvgAIPF <- genProductVTKAttr (255,255,255) (getCubicIPFColor . productAvgOrientation . snd) "AlphaIPF"
  let
    attrVoxAIPF = genVoxBoxAttr "VoxelAlpha" getCubicIPFColor orientationBox
    vtkD  = renderVoxBoxVTK grainIDBox attrs
    attrs = [ attrIPF, attrGID, attrMID, attrAvgErr
            , attrDevErr, attrMaxErr, attrLocErr
            , attrAvgAIPF, attrVoxAIPF ]
  liftIO $ plotVTK_D outputDir (vtkD,  name)

plotGraph :: Gomes ()
plotGraph = do
  GomesConfig{..} <- ask
  vtkGGraph <- plotGrainGraph
  liftIO $ plotVTK_V outputDir (vtkGGraph,  "GrainGraph")

run :: Deg -> FilePath -> FilePath -> IO ()
run miso fin fout = do
  ang <- parseANG fin
  let
    vbq = ebsdToVoxBox ang rotation
    ror = fromQuaternion $ mkQuaternion $ Vec4 7.126e-1 2.895e-1 2.238e-1 5.986e-1
    doit = do
      grainClustering
      --plotGraph
      --plotGroupSO3
      plotResults "1st-step"
      clusteringRefinement
      plotResults "2nd-step"
  _ <- case getGomesConfig fout miso 4 4 4 ror vbq of
    Nothing  -> error "No grain detected!"
    Just cfg -> runRWST doit cfg (getInitState cfg)
  return ()

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

calcProductGrains :: VoxBox Quaternion ->
               HashMap Int (V.Vector VoxelPos) ->
               HashMap Int ProductGrain
calcProductGrains vbq gmap = let
  getAvgQ = getQinFZ . shitQAvg . V.convert . V.map (vbq #!)
  getAvgPos v = let
    t = V.foldl' (&+) zero $ V.map (evalCentralVoxelPos vbq) v
    n = V.length v
    in t &* (1 / fromIntegral n)
  boxdim = dimension vbq
  func x = ProductGrain
           { productVoxelPos       = V.map (boxdim %@) x
           , productAvgOrientation = getAvgQ x
           , productAvgPos         = getAvgPos x
           }
  in HM.map func gmap

-- ================================== Grain clustering ===================================

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

getParentGrainData :: GomesConfig -> [Int] -> ParentGrain
getParentGrainData GomesConfig{..} mids = let
  getIS mid = maybe (V.empty) productVoxelPos (HM.lookup mid productGrains)
  getQ  mid = maybe zerorot productAvgOrientation (HM.lookup mid productGrains)
  -- Calculate parent's properties
  (gamma, err) = getWGammaTess realORs ws qs
  is = map (V.convert . getIS) mids
  qs = U.map getQ (U.fromList mids)
  ws = U.fromList (map (fromIntegral . U.length) is)
  wt = U.sum ws
  foo qi wi = let
    gerr = singleerrorfunc qi gamma realORs
    in (wi / wt, gerr)
  in ParentGrain
     { productMembers        = mids
     , parentOrientation     = gamma
     , parentAvgErrorFit     = err
     , parentErrorPerProduct = V.toList $ V.zipWith foo (U.convert qs) (V.convert ws)
     }

getGroupGrainsOrientations :: [Int] -> Gomes (Vector Double, Vector QuaternionFZ)
getGroupGrainsOrientations mids = do
  GomesConfig{..} <- ask
  GomesState{..}  <- get
  let
    getIS gid = maybe (V.empty) productVoxelPos (HM.lookup gid productGrains)
    getQ  gid = maybe zerorot productAvgOrientation (HM.lookup gid productGrains)
    is  = map (V.convert . getIS) mids
    ws  = U.fromList (map (fromIntegral . U.length) is)
    qs  = U.map getQ (U.fromList mids)
  return (ws, qs)

getWGammaTess :: Vector OR -> Vector Double -> Vector QuaternionFZ -> (Quaternion, FitError)
getWGammaTess ors ws qs = (gamma, err)
  where
    (g0, _, _) = hotStartTesseract ors qs
    errfunc = weightederrorfunc ws qs
    err     = errfunc gamma ors
    gamma   = findGamma errfunc g0 ors

getWGammaOR :: OR -> Vector Double -> Vector QuaternionFZ -> (Quaternion, FitError, OR)
getWGammaOR or0 ws qs = (gamma, err, rOR2)
  where
    errfunc = weightederrorfunc ws qs
    g0      = hotStartGamma errfunc
    err     = errfunc gamma (genTS or0)
    (gamma, rOR2) = findGammaOR errfunc g0 or0

-- ================================ Clustering Refinement ================================

clusteringRefinement :: Gomes ()
clusteringRefinement = do
  GomesConfig{..} <- ask
  st@GomesState{..}  <- get
  ps <- V.mapM refineParentGrain parentGrains
  put $ st { parentGrains = V.concatMap id ps }

goodParent :: ParentGrain -> Bool
goodParent ParentGrain{..} = badarea < 0.1
  where
    offlimit = filter ((> Deg 15) . snd) parentErrorPerProduct
    badarea  = sum $ map fst offlimit

getBadGrains :: ParentGrain -> [Int]
getBadGrains ParentGrain{..} = map fst bads
  where
    errs = zip productMembers parentErrorPerProduct
    bads = filter ((> Deg 15) . snd . snd) errs

reinforceCluster :: [Int] -> Graph Int Double -> Graph Int Double
reinforceCluster ns Graph{..} = let
  set  = HS.fromList ns
  foo k2 v2
    | HS.member k2 set = v2 * 2
    | otherwise        = v2
  func k1 v1
    | HS.member k1 set = HM.mapWithKey foo v1
    | otherwise        = v1
  in Graph $ HM.mapWithKey func graph

refineParentGrain :: ParentGrain -> Gomes (V.Vector ParentGrain)
refineParentGrain p@ParentGrain{..}
  | goodParent p = return (V.singleton p)
  | otherwise    = do
    cfg@GomesConfig{..} <- ask
    let
      graingraph  = getSubGraph productGraph productMembers
      badboys     = getBadGrains p
      graingraph2 = reinforceCluster badboys graingraph
    gg <- liftIO $ findClusters graingraph2 1.25
    liftIO $ print (gg, productMembers)
    return $ V.map (getParentGrainData cfg) gg

-- ========================================= MCL =========================================

saveGraph :: (Show a, Show b)=> Graph a b -> IO ()
saveGraph g = let
  xs = graphToList g
  func f ((n1,n2), x) = hPutStrLn f (unwords $ [show n1, show n2, show x])
  in do
    f <- openFile "test.txt" WriteMode
    mapM_ (func f) xs
    hClose f

runMCL :: Double -> IO ()
runMCL i = callCommand $ "mcl test.txt --abc -o test.out -I " ++ show i

readGroups :: IO (V.Vector [Int])
readGroups = readFile "test.out" >>= return . V.fromList . map (map read . words) . lines

findClusters :: Graph Int Double -> Double -> IO (V.Vector [Int])
findClusters g i = do
  saveGraph g
  runMCL i
  readGroups

-- ====================================== Plotting =======================================

plotVTK_D :: (RenderElemVTK a)=> String -> (VTK a, FilePath) -> IO ()
plotVTK_D dirout (vtk, name) = writeUniVTKfile (dirout ++ name  <.> "vtr") True vtk

plotVTK_V :: (RenderElemVTK a)=> String -> (VTK a, FilePath) -> IO ()
plotVTK_V dirout (vtk, name) = writeUniVTKfile (dirout ++ name  <.> "vtu") True vtk

genVoxBoxAttr :: (U.Unbox a, RenderElemVTK b)=>
                 String -> (a -> b) -> VoxBox a -> VTKAttrPoint c
genVoxBoxAttr name func qBox = mkPointAttr name (func . ((grainID qBox) U.!))

genProductVTKAttr :: (RenderElemVTK a, U.Unbox a, RenderElemVTK b)=>
                     a -> ((Int, ProductGrain) -> a) -> String -> Gomes (VTKAttrPoint b)
genProductVTKAttr nullvalue func name = do
  GomesConfig{..} <- ask
  GomesState{..}  <- get
  let
    fill v x@(gid, _) = V.mapM_ (\i -> MU.write v i (func x)) (getIS gid)
    nvox      = U.length $ grainID grainIDBox
    getIS gid = maybe (V.empty) productVoxelPos (HM.lookup gid productGrains)
    vec = U.create $ do
      v <- MU.replicate nvox nullvalue
      mapM_ (fill v) (HM.toList productGrains)
      return v
  return $ mkPointAttr name (vec U.!)

genLocalErrorVTKAttr :: (RenderElemVTK a, U.Unbox a, RenderElemVTK b)=>
                        a -> ((Double, Deg) -> a) -> String -> Gomes (VTKAttrPoint b)
genLocalErrorVTKAttr nullvalue func name = do
  GomesConfig{..} <- ask
  GomesState{..}  <- get
  let
    nvox      = U.length $ grainID grainIDBox
    getIS gid = maybe (V.empty) productVoxelPos (HM.lookup gid productGrains)
    fill m p = let
      is = map (V.convert . getIS) (productMembers p)
      foo v iv  = U.mapM_ (\i -> MU.write m i (func v)) iv
      in zipWithM_ foo (parentErrorPerProduct p) is
    vattr = U.create $ do
      m <- MU.replicate nvox nullvalue
      V.mapM_ (fill m) parentGrains
      return m
  return $ mkPointAttr name (vattr U.!)

genParentVTKAttr :: (RenderElemVTK a, U.Unbox a, RenderElemVTK b)=>
                    a -> ((Int, ParentGrain) -> a) -> String -> Gomes (VTKAttrPoint b)
genParentVTKAttr nullvalue func name = do
  GomesConfig{..} <- ask
  GomesState{..}  <- get
  let
    nvox      = U.length $ grainID grainIDBox
    getIS gid = maybe (V.empty) productVoxelPos (HM.lookup gid productGrains)
    fill m x@(_, p) = let
      is = U.concat $ map (V.convert . getIS) (productMembers p)
      in U.mapM_ (\i -> MU.write m i (func x)) is
    vattr = U.create $ do
      m <- MU.replicate nvox nullvalue
      V.mapM_ (fill m) (V.imap (,) parentGrains)
      return m
  return $ mkPointAttr name (vattr U.!)

plotGrainGraph :: Gomes (VTK Vec3)
plotGrainGraph = do
  GomesConfig{..} <- ask
  GomesState{..}  <- get
  let
    maxi = maximum $ HM.keys productGrains
    ps   = map (\(k, v) -> (k, productAvgPos v)) (HM.toList productGrains)
    ns   = U.replicate (maxi+1) zero U.// ps
    (es, vs) = unzip $ graphToList productGraph
    vtk = mkUGVTK "graph" ns es [] [attr]
    vv  = V.fromList vs
    attr = mkCellAttr "misoOR" (\i _ _ -> vv V.! i)
  return vtk

plotFitSO3 :: Quaternion -> [Int] -> Vector Quaternion -> Gomes (VTK Vec3, VTK Vec3, VTK Vec3)
plotFitSO3 gamma mids ms = do
  GomesConfig{..} <- ask
  GomesState{..}  <- get
  let
    ggid = U.singleton (mkGrainID $ -5)
    as   = U.map (toFZ Cubic . (gamma #<=) . qOR) realORs
    agid = U.replicate (U.length as) (mkGrainID $ -10)
    mgid = U.map mkGrainID $ U.fromList mids
    vtkSO3_m = renderSO3Points Cubic ND mgid ms
    vtkSO3_g = renderSO3Points Cubic ND ggid (U.singleton gamma)
    vtkSO3_a = renderSO3Points Cubic ND agid as
  return (vtkSO3_m, vtkSO3_g, vtkSO3_a)

plotHotSpotSO3 :: Quaternion -> [Int] -> Vector Quaternion -> Gomes (VTK Vec3, VTK Vec3, Vector Quaternion)
plotHotSpotSO3 gamma mids ms = do
  GomesConfig{..} <- ask
  GomesState{..}  <- get
  let
    func m = U.map (toFZ Cubic . (m #<=) . qOR) realORs
    ggid = U.singleton (mkGrainID $ -5)
    as   = U.concatMap func ms
    agid = U.replicate (U.length as) (mkGrainID $ -1)
    vtkSO3_a = renderSO3Points Cubic ND agid as
    vtkSO3_g = renderSO3Points Cubic ND ggid (U.singleton gamma)
  return (vtkSO3_g, vtkSO3_a, as)

plotGroupSO3 :: Gomes ()
plotGroupSO3 = do
  GomesConfig{..} <- ask
  GomesState{..}  <- get
  let
    foo vtk name gid = writeUniVTKfile ( outputDir ++ "gid_" ++
                                         show gid  ++ name <.> "vtu" ) True vtk
    fii vtk name gid = writeUniVTKfile ( outputDir ++ "gid_" ++
                                         show gid  ++ name <.> "vti" ) True vtk
    func (gid, mids) = do
      (ws, qs) <- getGroupGrainsOrientations mids
      let
        (g1, err1, _) = getWGammaOR       realOR  ws qs
        (g2, err2)    = getWGammaTess     realORs ws qs
        (g3, err3, t) = hotStartTesseract realORs qs
      liftIO $ putStrLn "---" >> print err1 >> print err2 >> print err3 >> print (g1,g2,g3) >> putStrLn "---"
      liftIO $ (fii (plotTesseract t) "-TESS" gid)
      --(vtk_m, vtk_g, vtk_a) <- plotFitSO3 gamma mids qs
      --(vtk_g, vtk_a, as) <- plotHotSpotSO3 gamma mids qs
      --liftIO $ saveText as gid
      --liftIO (foo vtk_m "-SO3-alpha"      gid)
      --liftIO (foo vtk_g "-SO3-gamma"      gid)
      --liftIO (foo vtk_a "-SO3-simu_alpha" gid)
  --V.mapM_ func $ V.imap (,) parentGrains
  return()

getCubicIPFColor :: (Rot q)=> q -> (Word8, Word8, Word8)
getCubicIPFColor = let
  unColor (RGBColor rgb) = rgb
  in unColor . getRGBColor . snd . getIPFColor Cubic ND . convert
