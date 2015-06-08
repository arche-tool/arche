{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}

module Gamma.Strategy.GomesGraph
       ( run
       , Cfg(..)
       ) where

import qualified Data.Vector                  as V
import qualified Data.Vector.Unboxed          as U
import qualified Data.Vector.Unboxed.Mutable  as MU
import qualified Data.HashMap.Strict          as HM
import qualified Data.HashSet                 as HS
import qualified Data.List                    as L

import           Control.Applicative ((<$>))
import           Control.Monad       (when, replicateM_, zipWithM_)
import           Control.Monad.RWS   (RWST(..), ask, get, put, runRWST)
import           Data.Vector.Unboxed (Vector)
import           Data.HashMap.Strict (HashMap)
import           Data.Word           (Word8)
import           Data.Maybe          (mapMaybe)

import           System.FilePath
import           System.IO
import           System.Process
import           Control.Parallel.Strategies
import           Control.Monad.Trans

import           Hammer.Math.Algebra
import           Hammer.VoxBox
import           Hammer.VTK.VoxBox
import           Hammer.VTK
import           Hammer.Graph
import           Hammer.MicroGraph

import qualified File.ANGReader as A
import qualified File.CTFReader as C
import           File.ANGReader (ANGdata)
import           File.CTFReader (CTFdata)
import           File.ANGWriter
import           File.CTFWriter
import           File.EBSD
import           Texture.Symmetry    (Symm (..))
import           Texture.IPF
import           Texture.Orientation
import           Texture.ODF

import           Gamma.Grains
import           Gamma.OR

--import Debug.Trace
--dbg a = trace (show a) a
--dbgs s a = trace (show s ++ " <=> " ++ show a) a

data Cfg =
  Cfg
  { misoAngle              :: Deg
  , ang_input              :: FilePath
  , base_output            :: FilePath
  , useExternalMCL         :: Bool
  , excluedeFloatingGrains :: Bool
  , refinementSteps        :: Word8
  , initClusterFactor      :: Double
  , stepClusterFactor      :: Double
  , badAngle               :: Deg
  , withOR                 :: AxisPair
  , gammaPhaseID           :: Int
  , outputANGMap           :: Bool
  , outputCTFMap           :: Bool
  } deriving (Show)

data ProductGrain =
  ProductGrain
  { productVoxelPos       :: V.Vector Int
  , productAvgOrientation :: QuaternionFZ
  , productAvgPos         :: Vec3
  , productPhase          :: Int
  } deriving (Show)

data ParentGrain =
  ParentGrain
  { productMembers        :: [Int]
  , parentOrientation     :: Quaternion
  , parentAvgErrorFit     :: FitError
  , parentErrorPerProduct :: [ParentProductFit]
  } deriving (Show)

data ParentProductFit
  = ParentProductFit
  { areaFraction  :: {-# UNPACK #-} !Double
  , misfitAngle   :: Deg
  , variantNumber :: {-# UNPACK #-} !Int
  } deriving (Show)


data GomesConfig
  = GomesConfig
  { inputCfg          :: Cfg
  , realOR            :: OR
  , realORs           :: Vector OR
  , inputEBSD         :: Either ANGdata CTFdata
  , orientationBox    :: VoxBox (Quaternion, Int)
  , grainIDBox        :: VoxBox GrainID
  , structureGraph    :: MicroVoxel
  , productGrains     :: HashMap Int ProductGrain
  , productGraph      :: Graph Int Double
  , orientationGrid   :: ODF
  }

data GomesState
  = GomesState
  { parentGrains :: V.Vector ParentGrain
  , mclFactor    :: Double
  } deriving (Show)

type Gomes = RWST GomesConfig () GomesState IO

-- =======================================================================================

getGomesConfig :: Cfg -> OR -> Either ANGdata CTFdata -> Maybe GomesConfig
getGomesConfig cfg ror ebsd = let
  noIsleGrains = excluedeFloatingGrains cfg
  qpBox = readEBSDToVoxBox
          (\p -> (C.rotation p, C.phase p   ))
          (\p -> (A.rotation p, A.phaseNum p))
          ebsd
  func (gidBox, voxMap) = let
    micro = fst $ getMicroVoxel (gidBox, voxMap)
    in GomesConfig
     { inputCfg       = cfg
     , realOR         = ror
     , realORs        = genTS ror
     , inputEBSD      = ebsd
     , orientationBox = qpBox
     , grainIDBox     = gidBox
     , productGrains  = getProductGrainData qpBox voxMap
     , structureGraph = micro
     , productGraph   = graphWeight noIsleGrains qpBox micro ror
     , orientationGrid = buildEmptyODF (Deg 2.5) Cubic (Deg 2.5)
     }
  in fmap func (getGrainID' (misoAngle cfg) Cubic qpBox)

getInitState :: GomesConfig -> GomesState
getInitState GomesConfig{..} = let
  k = initClusterFactor inputCfg
  in GomesState { parentGrains = V.empty
                , mclFactor    = k
                }

grainClustering :: Gomes ()
grainClustering = do
  cfg@GomesConfig{..} <- ask
  st@GomesState{..}   <- get
  gg <- liftIO $ findClusters productGraph mclFactor (useExternalMCL inputCfg)
  let ps = V.map (getParentGrainData cfg) gg
  put $ st { parentGrains = ps
           , mclFactor    = mclFactor * stepClusterFactor inputCfg
           }

plotResults :: String -> Gomes ()
plotResults name = do
  plotVTK  name
  plotEBSD name

plotVTK :: String -> Gomes ()
plotVTK name = do
  GomesConfig{..} <- ask
  attrGID    <- genParentVTKAttr (-1) fst "ParentGrainID"
  attrIPF    <- genParentVTKAttr (255,255,255) (getCubicIPFColor . parentOrientation . snd) "GammaIPF"
  attrAvgErr <- genParentVTKAttr (-1) (unDeg . avgError . parentAvgErrorFit . snd) "AvgError"
  attrDevErr <- genParentVTKAttr (-1) (unDeg . devError . parentAvgErrorFit . snd) "DevError"
  attrMaxErr <- genParentVTKAttr (-1) (unDeg . maxError . parentAvgErrorFit . snd) "MaxError"

  attrMID    <- genLocalErrorVTKAttr (-1) areaFraction          "ProductGrainAreaFraction"
  attrORVar  <- genLocalErrorVTKAttr (-1) variantNumber         "VariantNumber"
  attrLocErr <- genLocalErrorVTKAttr (-1) (unDeg . misfitAngle) "ErrorPerProduct"

  attrAvgAIPF <- genProductVTKAttr (255,255,255) (getCubicIPFColor . productAvgOrientation . snd) "AlphaIPF"
  let
    attrVoxAIPF  = genVoxBoxAttr "VoxelAlpha" (getCubicIPFColor . fst) orientationBox
    attrVoxPhase = genVoxBoxAttr "Phases" snd orientationBox
    vtkD  = renderVoxBoxVTK grainIDBox attrs
    attrs = [ attrIPF, attrGID, attrMID, attrAvgErr
            , attrDevErr, attrMaxErr, attrLocErr
            , attrAvgAIPF, attrVoxAIPF, attrVoxPhase
            , attrORVar ]
  liftIO $ plotVTK_D inputCfg (vtkD,  name)

plotEBSD :: String -> Gomes ()
plotEBSD name = do
  Cfg{..} <- fmap inputCfg ask
  let file = base_output ++ name
  ebsd <- genParentEBSD
  liftIO $ do
    when outputANGMap $ renderANGFile (file  <.> "ang") $ either id toANG ebsd
    when outputCTFMap $ renderCTFFile (file  <.> "ctf") $ either toCTF id ebsd

run :: Cfg -> IO ()
run cfg@Cfg{..} = do
  ebsd <- readEBSD ang_input
  let
    ror  = convert withOR
    nref = fromIntegral refinementSteps
    doit = do
      grainClustering
      plotResults "1st-step"
      replicateM_ nref clusteringRefinement
      plotResults "final-step"
  gomescfg <- maybe (error "No grain detected!") return (getGomesConfig cfg ror ebsd)
  putStrLn $ "[GomesGraph] Using OR = " ++ show ((fromQuaternion $ qOR ror) :: AxisPair)
  runRWST doit gomescfg (getInitState gomescfg) >> return ()

-- ==================================== Initial Graph ====================================

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

getFaceIDmisOR :: VoxBox (Quaternion, Int) -> MicroVoxel -> Vector OR
               -> FaceID -> Maybe Double
getFaceIDmisOR vbq micro ors fid = let
  facelist = getFaceProp fid micro >>= getPropValue
  func fs = let
    t = V.sum $ V.map (getFaceVoxelmisOR vbq ors) fs
    n = fromIntegral $ V.length fs
    in (t / n) :: Double
  in func <$> facelist

getFaceVoxelmisOR :: VoxBox (Quaternion, Int) -> Vector OR -> FaceVoxelPos -> Double
getFaceVoxelmisOR vbq ors face = let
  (v1, v2) = getFaceVoxels face
  q1 = vbq #! v1
  q2 = vbq #! v2
  in evalMisoOR ors q1 q2

-- TODO move to Hammer
-- | Get both voxels that forms a given face.
getFaceVoxels :: FaceVoxelPos -> (VoxelPos, VoxelPos)
getFaceVoxels (Fx pos) = (pos, pos #+# (VoxelPos (-1) 0 0))
getFaceVoxels (Fy pos) = (pos, pos #+# (VoxelPos 0 (-1) 0))
getFaceVoxels (Fz pos) = (pos, pos #+# (VoxelPos 0 0 (-1)))

graphWeight :: Bool -> VoxBox (Quaternion, Int) -> MicroVoxel -> OR -> Graph Int Double
graphWeight noIsleGrains vbq micro withOR = let
  fs    = HM.keys $ microFaces micro
  ors   = genTS withOR
  ms    = map (getFaceIDmisOR vbq micro ors) fs
  weight x = let
    k = -300
    in exp (k * x * x)
  mspar = ms `using` parListChunk 100 rpar
  func  = if noIsleGrains then filterIsleGrains else id
  in func . mkUniGraph [] . filter ((>= 0) . snd) $
     zipWith (\fid x -> (unFaceID fid, maybe 0 weight x)) fs mspar

-- ================================== Grain Data ===================================

getProductGrainData :: VoxBox (Quaternion, Int) ->
                       HashMap Int (V.Vector VoxelPos) ->
                       HashMap Int ProductGrain
getProductGrainData vbq gmap = let
  getAvgQ = getQinFZ . shitQAvg . V.convert . V.map (fst . (vbq #!))
  getAvgPos v = let
    t = V.foldl' (&+) zero $ V.map (evalCentralVoxelPos vbq) v
    n = V.length v
    in t &* (1 / fromIntegral n)
  boxdim = dimension vbq
  func gid x = ProductGrain
           { productVoxelPos       = V.map (boxdim %@) x
           , productAvgOrientation = getAvgQ x
           , productAvgPos         = getAvgPos x
           , productPhase          = maybe (-1) id (getGrainPhase vbq gmap gid)
           }
  in HM.mapWithKey func gmap

getParentGrainData :: GomesConfig -> [Int] -> ParentGrain
getParentGrainData GomesConfig{..} mids = let
  getInfo mid = HM.lookup mid productGrains >>= \ProductGrain{..} ->
    return (fromIntegral $ V.length productVoxelPos, productAvgOrientation, productPhase)
  -- Calculate parent's properties
  info = U.fromList $ mapMaybe getInfo mids
  wt   = U.foldl' (\acc (w,_,_) -> acc + w) 0 info
  --(gamma, err) = getWGammaTess (gammaPhaseID inputCfg) realORs info
  (gamma, err) = getWGammaKernel orientationGrid (gammaPhaseID inputCfg) realORs info
  foo :: (Double, QuaternionFZ, Int) -> ParentProductFit
  foo (wi, qi, _) = let
    (gerr, nvar) = singleerrorfunc qi gamma realORs
    in ParentProductFit (wi / wt) gerr nvar
  in ParentGrain
     { productMembers        = mids
     , parentOrientation     = gamma
     , parentAvgErrorFit     = err
     , parentErrorPerProduct = map foo (U.toList info)
     }

-- | Find the parent orientation from an set of products and remained parents. It takes
-- an set of symmetric equivalent ORs, a list of weights for each grain, list remained
-- parent orientation and a list of product orientations.
getWGammaKernel :: ODF -> Int -> Vector OR -> Vector (Double, QuaternionFZ, Int) -> (Quaternion, FitError)
getWGammaKernel odf phaseID ors xs = (gamma, err)
  where
    (was, wms) = U.partition (\(_,_,p) -> p == phaseID) xs
    (gamma, err) = gammaFinderKernel odf ors as ms
    (_, as, _) = U.unzip3 was
    (_, ms, _) = U.unzip3 wms

-- | Find the parent orientation from an set of products and remained parents. It takes
-- an set of symmetric equivalent ORs, a list of weights for each grain, list remained
-- parent orientation and a list of product orientations.
getWGammaTess :: Int -> Vector OR -> Vector (Double, QuaternionFZ, Int) -> (Quaternion, FitError)
getWGammaTess phaseID ors xs = (gamma, err)
  where
    (gs, as) = U.partition (\(_,_,p) -> p == phaseID) xs
    g0 | U.null gs = let (g, _, _) = hotStartTesseract ors qs in g
       | otherwise = shitQAvg $ U.map (\(_,q,_) -> qFZ q) gs
    (ws, qs, _) = U.unzip3 as
    errfunc = weightederrorfunc ws qs
    err     = errfunc gamma ors
    gamma   = findGamma errfunc g0 ors

-- ================================ Clustering Refinement ================================

clusteringRefinement :: Gomes ()
clusteringRefinement = do
  GomesConfig{..}   <- ask
  st@GomesState{..} <- get
  ps <- V.mapM refineParentGrain parentGrains
  put $ st { parentGrains = V.concatMap id ps
           , mclFactor    = mclFactor * stepClusterFactor inputCfg
           }

goodParent :: Deg -> ParentGrain -> Bool
goodParent badW ParentGrain{..} = badarea < 0.1
  where
    offlimit = filter ((> badW) . misfitAngle) parentErrorPerProduct
    badarea  = sum $ map areaFraction offlimit

getBadGrains :: Deg -> ParentGrain -> [Int]
getBadGrains badW ParentGrain{..} = map fst bads
  where
    errs = zip productMembers parentErrorPerProduct
    bads = filter ((> badW) . misfitAngle . snd) errs

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
refineParentGrain p@ParentGrain{..} = do
  cfg@GomesConfig{..} <- ask
  GomesState{..}      <- get
  let
    badFitAngle  = badAngle inputCfg
    graingraph   = getSubGraph productGraph productMembers
    badboys      = getBadGrains badFitAngle p
    graingraph2  = reinforceCluster badboys graingraph
  if goodParent badFitAngle p
    then return (V.singleton p)
    else do
    gg <- liftIO $ findClusters graingraph2 mclFactor (useExternalMCL inputCfg)
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

runMCLIO :: Double -> IO ()
runMCLIO i = callCommand $ "mcl test.txt --abc -o test.out -I " ++ show i

readGroups :: IO (V.Vector [Int])
readGroups = readFile "test.out" >>= return . V.fromList . map (map read . words) . lines

findClusters :: Graph Int Double -> Double -> Bool -> IO (V.Vector [Int])
findClusters graph0 i extMCL
  | null (graphToList graph0) = putStrLn "Warning: attempting to cluster the void" >> return (V.empty)
  | extMCL                    = saveGraph graph0 >> runMCLIO i >> readGroups
  | otherwise = let
    cfgMCL = defaultMCL {inflation = i, selfLoop = 0.5}
    in return (V.fromList $ runMCL cfgMCL graph0)

-- ====================================== Pixalization functions =======================================

genProductGrainBitmap :: (U.Unbox a)=> a -> ((Int, ProductGrain) -> a) -> Gomes (U.Vector a)
genProductGrainBitmap nullvalue func = do
  GomesConfig{..} <- ask
  GomesState{..}  <- get
  let
    nvox      = U.length $ grainID grainIDBox
    getIS gid = maybe V.empty productVoxelPos (HM.lookup gid productGrains)
    fill v x@(gid, _) = V.mapM_ (\i -> MU.write v i (func x)) (getIS gid)
  return $ U.create $ do
    v <- MU.replicate nvox nullvalue
    mapM_ (fill v) (HM.toList productGrains)
    return v

genParentProductFitBitmap :: (U.Unbox a)=> a -> (ParentProductFit -> a) -> Gomes (U.Vector a)
genParentProductFitBitmap nullvalue func = do
  GomesConfig{..} <- ask
  GomesState{..}  <- get
  let
    nvox      = U.length $ grainID grainIDBox
    getIS gid = maybe V.empty productVoxelPos (HM.lookup gid productGrains)
    fill m p = let
      is = map (V.convert . getIS) (productMembers p)
      foo v iv  = U.mapM_ (\i -> MU.write m i (func v)) iv
      in zipWithM_ foo (parentErrorPerProduct p) is
  return $ U.create $ do
    m <- MU.replicate nvox nullvalue
    V.mapM_ (fill m) parentGrains
    return m

genParentGrainBitmap :: (U.Unbox a)=> a -> ((Int, ParentGrain) -> a) -> Gomes (U.Vector a)
genParentGrainBitmap nullvalue func = do
  GomesConfig{..} <- ask
  GomesState{..}  <- get
  let
    nvox      = U.length $ grainID grainIDBox
    getIS gid = maybe (V.empty) productVoxelPos (HM.lookup gid productGrains)
    fill m x@(_, p) = let
      is = U.concat $ map (V.convert . getIS) (productMembers p)
      in U.mapM_ (\i -> MU.write m i (func x)) is
  return $ U.create $ do
    m <- MU.replicate nvox nullvalue
    V.mapM_ (fill m) (V.imap (,) parentGrains)
    return m

-- ====================================== Plotting VTK ===================================================

plotVTK_D :: (RenderElemVTK a)=> Cfg -> (VTK a, String) -> IO ()
plotVTK_D Cfg{..} (vtk, name) = writeUniVTKfile (base_output ++ name  <.> "vtr") True vtk

genVoxBoxAttr :: (U.Unbox a, RenderElemVTK b)=> String -> (a -> b) -> VoxBox a -> VTKAttrPoint c
genVoxBoxAttr name func qBox = mkPointAttr name (func . ((grainID qBox) U.!))

genProductVTKAttr :: (RenderElemVTK a, U.Unbox a, RenderElemVTK b)=> a -> ((Int, ProductGrain) -> a) -> String -> Gomes (VTKAttrPoint b)
genProductVTKAttr nul func name = (mkPointAttr name . (U.!)) <$> genProductGrainBitmap nul func

genLocalErrorVTKAttr :: (RenderElemVTK a, U.Unbox a, RenderElemVTK b)=> a -> (ParentProductFit -> a) -> String -> Gomes (VTKAttrPoint b)
genLocalErrorVTKAttr nul func name = (mkPointAttr name . (U.!))<$> genParentProductFitBitmap nul func

genParentVTKAttr :: (RenderElemVTK a, U.Unbox a, RenderElemVTK b)=> a -> ((Int, ParentGrain) -> a) -> String -> Gomes (VTKAttrPoint b)
genParentVTKAttr nul f name = func <$> genParentGrainBitmap nul f
  where func = mkPointAttr name . (U.!)

getCubicIPFColor :: (Rot q)=> q -> (Word8, Word8, Word8)
getCubicIPFColor = let
  unColor (RGBColor rgb) = rgb
  in unColor . getRGBColor . snd . getIPFColor Cubic ND . convert

-- ====================================== Plotting EBSD/CTF =============================================

genParentEBSD :: Gomes (Either ANGdata CTFdata)
genParentEBSD = ask >>= either (fmap Left . genParentANG) (fmap Right . genParentCTF) . inputEBSD

genParentANG :: ANGdata -> Gomes ANGdata
genParentANG ang = do
  qs <- U.convert <$> genParentGrainBitmap zerorot (parentOrientation . snd)
  vs <- U.convert <$> genParentProductFitBitmap 0 variantNumber
  return $ ang {A.nodes = V.zipWith3 ins qs vs (A.nodes ang)}
  where ins q v p = p {A.rotation = q, A.ci = fromIntegral v / 24}

genParentCTF :: CTFdata -> Gomes CTFdata
genParentCTF ang = func . U.convert <$> genParentGrainBitmap zerorot (parentOrientation . snd)
  where func qs = ang {C.nodes = V.zipWith insRotation qs (C.nodes ang)}
        insRotation q p = p {C.rotation = q}
