{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , FlexibleInstances
  , RecordWildCards
  , OverloadedStrings
  , ScopedTypeVariables
  #-}

module Arche.Strategy.GomesGraph
  ( run
  , processEBSD
  , Cfg(..)
  , renderImage
  ) where

import Codec.Picture.Png     (encodePng)
import Codec.Picture.Types   (PixelRGB8(..), Image, generateImage)
import Control.Arrow         ((&&&))
import Control.DeepSeq       (NFData)
import Control.Monad         (forM_, when, replicateM_, zipWithM_, void)
import Control.Monad.RWS     (RWST(..), ask, asks, get, put, runRWST)
import Control.Monad.Trans
import Control.Parallel.Strategies
import Data.HashMap.Strict   (HashMap)
import Data.Maybe            (mapMaybe, fromMaybe)
import Data.TDigest          (TDigest)
import Data.Vector.Unboxed   (Vector)
import Data.Word             (Word8)
import GHC.Generics          (Generic)
import System.FilePath
import System.IO
import System.Log.FastLogger (LoggerSet, ToLogStr)
import System.Process
import Text.Printf (printf)
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.Vector                  as V
import qualified Data.Vector.Unboxed          as U
import qualified Data.Vector.Unboxed.Mutable  as MU
import qualified Data.HashMap.Strict          as HM
import qualified Data.HashSet                 as HS
import qualified Data.List                    as L
import qualified Data.Text.Format             as F
import qualified Data.TDigest                 as TD
import qualified System.Log.FastLogger        as Log

import File.ANGReader (ANGdata)
import File.CTFReader (CTFdata)
import File.ANGWriter
import File.CTFWriter
import File.EBSD
import Hammer.VoxBox
import Hammer.VTK
import Hammer.Graph
import Hammer.MicroGraph
import Linear.Vect
import Texture.IPF
import Texture.Orientation
import Texture.ODF
import Texture.Symmetry
import Texture.HyperSphere
import qualified File.ANGReader as A
import qualified File.CTFReader as C

import Arche.Grains
import Arche.OR

data Cfg =
  Cfg
  { misoAngle              :: Deg
  , useExternalMCL         :: Bool
  , excludeFloatingGrains  :: Bool
  , refinementSteps        :: Word8
  , initClusterFactor      :: Double
  , stepClusterFactor      :: Double
  , badAngle               :: Deg
  , withOR                 :: OR
  , parentPhaseID          :: Maybe PhaseID
  , outputANGMap           :: Bool
  , outputCTFMap           :: Bool
  } deriving (Show, Generic)

data ProductGrain =
  ProductGrain
  { productVoxelPos       :: V.Vector Int
  , productAvgOrientation :: QuaternionFZ
  , productAvgPos         :: Vec3D
  , productPhase          :: PhaseID
  } deriving (Show)

data ParentGrain =
  ParentGrain
  { productMembers        :: [Int]
  , parentOrientation     :: Quaternion
  , parentAvgErrorFit     :: FitError
  , parentErrorPerProduct :: [ParentProductFit]
  } deriving (Generic, Show)

data ParentProductFit
  = ParentProductFit
  { areaFraction  :: !Double
  , misfitAngle   :: !Deg
  , variantNumber :: !(Int, OR)
  } deriving (Generic, Show)

data GomesConfig
  = GomesConfig
  { inputCfg        :: Cfg
  , realOR          :: OR
  , realORs         :: Vector OR
  , inputEBSD       :: EBSDdata
  , orientationBox  :: VoxBox (Quaternion, Int)
  , grainIDBox      :: VoxBox GrainID
  , structureGraph  :: MicroVoxel
  , productGrains   :: HashMap Int ProductGrain
  , productGraph    :: Graph Int Double
  , orientationGrid :: ODF
  , stdoutLogger    :: LoggerSet
  }

data GomesState
  = GomesState
  { parentGrains :: V.Vector ParentGrain
  , mclFactor    :: Double
  } deriving (Show)

type Gomes = RWST GomesConfig () GomesState

instance NFData ParentGrain
instance NFData ParentProductFit

-- =======================================================================================

logInfo :: (MonadIO m, Monad m, ToLogStr a)=> a -> Gomes m ()
logInfo msg = asks stdoutLogger >>= (\logger -> liftIO . Log.pushLogStrLn logger . Log.toLogStr $ msg) 

getDist :: (a -> Double) -> V.Vector a -> TDigest 10
getDist func = TD.compress . V.foldl (\td x -> TD.insert (func x) td) mempty

logParentStatsState :: (MonadIO m, Monad m)=> Gomes m ()
logParentStatsState = get >>= logParentStats . parentGrains

logParentStats :: (MonadIO m, Monad m)=> V.Vector ParentGrain -> Gomes m ()
logParentStats parents = let
  tdNumProducts = getDist (fromIntegral . length . productMembers) parents
  renderMsg q = let
    v = maybe 0.0 id $ TD.quantile (fromIntegral q / 100.0) tdNumProducts
    in F.format "{} products per parent grain at {}% quantile." $ (F.prec 2 v, F.left 2 '0' q)
  in do
    logInfo ("[GomesGraph] " <> renderMsg (5 :: Int))
    logInfo ("[GomesGraph] " <> renderMsg (25 :: Int))
    logInfo ("[GomesGraph] " <> renderMsg (50 :: Int))
    logInfo ("[GomesGraph] " <> renderMsg (75 :: Int))
    logInfo ("[GomesGraph] " <> renderMsg (95 :: Int))

-- =======================================================================================

getGomesConfig :: Cfg -> OR -> Either String EBSDdata -> LoggerSet -> Either String GomesConfig
getGomesConfig cfg ror maybeEBSD logger = do
  ebsd  <- maybeEBSD 
  qpBox <- readEBSDToVoxBox
          (C.rotation &&& C.phase)
          (A.rotation &&& A.phaseNum)
          ebsd
  let 
    noIsleGrains = excludeFloatingGrains cfg
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
        , stdoutLogger   = logger
        }
  maybe (Left "No grain detected!") (Right . func) (getGrainID' (misoAngle cfg) Cubic qpBox)

getInitState :: GomesConfig -> GomesState
getInitState GomesConfig{..} = let
  k = initClusterFactor inputCfg
  in GomesState { parentGrains = V.empty
                , mclFactor    = k
                }

grainClustering :: (MonadIO m, Monad m)=> Gomes m ()
grainClustering = do
  cfg@GomesConfig{..} <- ask
  st@GomesState{..}   <- get
  gg <- liftIO $ findClusters productGraph mclFactor (useExternalMCL inputCfg)
  let ps = V.map (getParentGrainData cfg) gg
  put $ st { parentGrains = ps
           , mclFactor    = mclFactor * stepClusterFactor inputCfg
           }

getTransformedProduct :: GomesConfig -> (Quaternion, PhaseID) -> ParentGrain -> Quaternion
getTransformedProduct cfg (q, phase) p
  | Just phase == phaseRef = q
  | otherwise              = findBestTransformation ors (parentOrientation p) q
  where
    ors   = realORs cfg
    phaseRef = parentPhaseID . inputCfg $ cfg

findBestTransformation ::  Vector OR -> Quaternion -> Quaternion -> Quaternion
findBestTransformation ors ref q = let
  qs  = U.map ((qFZ (getQinFZ q) #<=) . qOR) ors
  ms  = U.map (fst . splitQuaternion . qFZ . getQinFZ . (-#- ref)) qs
  i   = U.maxIndex ms
  in qFZ . getQinFZ $ qs U.! i

run :: Cfg -> FilePath -> FilePath -> IO ()
run cfg ebsd_file base_output = do
  bs <- BSL.readFile ebsd_file
  let savePlot = get >>= plotResults . (base_output <>) . printf "mcl_%.2f" . mclFactor
  void $ processEBSD cfg bs savePlot

processEBSD :: (MonadIO m, Monad m)=> Cfg -> BSL.ByteString -> Gomes m () -> m GomesState
processEBSD cfg@Cfg{..} bs action = do
  logger <- liftIO $ Log.newStdoutLoggerSet Log.defaultBufSize
  let
    ebsd = loadEBSD bs
    ror  = withOR
    nref = fromIntegral refinementSteps
    doit = do
      grainClustering
      action
      logParentStatsState
      replicateM_ nref $ do
        clusteringRefinement
        logParentStatsState
        action
  gomescfg <- either error return (getGomesConfig cfg ror ebsd logger)
  liftIO . putStrLn $ "[GomesGraph] Using OR = " ++ show ((fromQuaternion $ qOR ror) :: AxisPair)
  (_, final, _) <- runRWST doit gomescfg (getInitState gomescfg)
  return final

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
getFaceVoxels (Fx pos) = (pos, pos #+# VoxelPos (-1) 0 0)
getFaceVoxels (Fy pos) = (pos, pos #+# VoxelPos 0 (-1) 0)
getFaceVoxels (Fz pos) = (pos, pos #+# VoxelPos 0 0 (-1))

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

getProductGrainData :: VoxBox (Quaternion, Int) -> HashMap Int (V.Vector VoxelPos) -> HashMap Int ProductGrain
getProductGrainData vbq gmap = let
  getAvgQ = getQinFZ . averageQuaternionWithSymm Cubic . V.map (fst . (vbq #!))
  getAvgPos v = let
    t = V.foldl' (&+) zero $ V.map (evalCentralVoxelPos vbq) v
    n = V.length v
    in t &* (1 / fromIntegral n)
  boxdim = dimension vbq
  func gid x = ProductGrain
           { productVoxelPos       = V.map (boxdim %@) x
           , productAvgOrientation = getAvgQ x
           , productAvgPos         = getAvgPos x
           , productPhase          = PhaseID $ fromMaybe (-1) (getGrainPhase vbq gmap gid)
           }
  in HM.mapWithKey func gmap

getParentGrainData :: GomesConfig -> [Int] -> ParentGrain
getParentGrainData GomesConfig{..} mids = let
  getInfo mid = HM.lookup mid productGrains >>= \ProductGrain{..} ->
    return (fromIntegral . V.length $ productVoxelPos, productAvgOrientation, productPhase)

  -- Calculate parent's properties
  info = U.fromList $ mapMaybe getInfo mids
  wt   = U.foldl' (\acc (w,_,_) -> acc + w) 0 info

  --(arche, err) = getWArcheTess (parentPhaseID inputCfg) realORs info
  (arche, err) = getWArcheKernel orientationGrid (parentPhaseID inputCfg) realORs info

  getFitInfo :: (Double, QuaternionFZ, PhaseID) -> ParentProductFit
  getFitInfo (wi, qi, phase)
    | Just phase == parentPhaseID inputCfg = ParentProductFit (wi / wt) 0 (-1, mempty)
    | otherwise                            = ParentProductFit (wi / wt) gerr nvar
    where
      (gerr, nvar) = singleerrorfunc qi arche realORs

  in ParentGrain
     { productMembers        = mids
     , parentOrientation     = arche
     , parentAvgErrorFit     = err
     , parentErrorPerProduct = map getFitInfo (U.toList info)
     }

-- | Find the parent orientation from an set of products and remained parents. It takes
-- an set of symmetric equivalent ORs, a list of weights for each grain, list remained
-- parent orientation and a list of product orientations.
getWArcheKernel :: ODF -> Maybe PhaseID -> Vector OR -> Vector (Double, QuaternionFZ, PhaseID) -> (Quaternion, FitError)
getWArcheKernel odf phaseID ors xs = (arche, err)
  where
    (was, wms) = U.partition (\(_,_,p) -> Just p == phaseID) xs
    (arche, err) = archeFinderKernel odf ors as ms
    (_, as, _) = U.unzip3 was
    (_, ms, _) = U.unzip3 wms

-- | Find the parent orientation from an set of products and remained parents. It takes
-- an set of symmetric equivalent ORs, a list of weights for each grain, list remained
-- parent orientation and a list of product orientations.
-- TODO: Remove
_getWArcheTess :: Int -> Vector OR -> Vector (Double, QuaternionFZ, Int) -> (Quaternion, FitError)
_getWArcheTess phaseID ors xs = (arche, err)
  where
    (gs, as) = U.partition (\(_,_,p) -> p == phaseID) xs
    g0 | U.null gs = let (g, _, _) = hotStartTesseract ors qs in g
       | otherwise = averageQuaternion (V.convert $ U.map (\(_,q,_) -> qFZ q) gs :: V.Vector Quaternion)
    (ws, qs, _) = U.unzip3 as
    errfunc = weightederrorfunc ws qs
    err     = errfunc arche ors
    arche   = findArche errfunc g0 ors

-- ================================ Clustering Refinement ================================

clusteringRefinement :: (MonadIO m, Monad m)=> Gomes m ()
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

refineParentGrain :: (MonadIO m, Monad m)=> ParentGrain -> Gomes m (V.Vector ParentGrain)
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
    -- logInfo $ show (gg, productMembers)
    return $ V.map (getParentGrainData cfg) gg

-- ========================================= MCL =========================================

saveGraph :: (Show a, Show b)=> Graph a b -> IO ()
saveGraph g = let
  xs = graphToList g
  func f ((n1,n2), x) = hPutStrLn f $ unwords [show n1, show n2, show x]
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
  | null (graphToList graph0) = putStrLn "Warning: attempting to cluster the void" >> return V.empty
  | extMCL                    = saveGraph graph0 >> runMCLIO i >> readGroups
  | otherwise = let
    cfgMCL = defaultMCL {inflation = i, selfLoop = 0.5}
    in return (V.fromList $ runMCL cfgMCL graph0)

-- ====================================== Pixalization functions =======================================

genProductGrainBitmap :: (Monad m)=> (U.Unbox a)=> a -> ((Int, ProductGrain) -> a) -> Gomes m (U.Vector a)
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

genParentProductFitBitmap :: (Monad m)=> (U.Unbox a)=> a -> ((Quaternion, PhaseID) -> ParentGrain -> ParentProductFit -> a) -> Gomes m (U.Vector a)
genParentProductFitBitmap nullvalue func = do
  GomesConfig{..} <- ask
  GomesState{..}  <- get
  let
    nvox    = U.length . grainID $ grainIDBox
    getProd = fmap (V.convert . productVoxelPos &&& productPhase) . flip HM.lookup productGrains
    fillAllGrains m p = zipWithM_ fillSingleGrain (parentErrorPerProduct p) vis
      where
      vis  = mapMaybe getProd (productMembers p)
      getQ = fst . (grainID orientationBox U.!)
      fillSingleGrain v (is, ph) = U.mapM_ (\i -> MU.write m i (func (getQ i, ph) p v)) is
  return $ U.create $ do
    m <- MU.replicate nvox nullvalue
    V.mapM_ (fillAllGrains m) parentGrains
    return m

genParentGrainBitmap :: (Monad m)=> (U.Unbox a)=> a -> ((Int, ParentGrain) -> a) -> Gomes m (U.Vector a)
genParentGrainBitmap nullvalue func = do
  GomesConfig{..} <- ask
  GomesState{..}  <- get
  let
    nvox      = U.length $ grainID grainIDBox
    getIS gid = maybe V.empty productVoxelPos (HM.lookup gid productGrains)
    fill m x@(_, p) = let
      is = U.concat $ map (V.convert . getIS) (productMembers p)
      in U.mapM_ (\i -> MU.write m i (func x)) is
  return $ U.create $ do
    m <- MU.replicate nvox nullvalue
    V.mapM_ (fill m) (V.imap (,) parentGrains)
    return m

getVoxBoxRange :: (Monad m)=> Gomes m VoxBoxRange
getVoxBoxRange = (dimension . grainIDBox) <$> ask

toPixelRGB8 :: (Word8, Word8, Word8) -> PixelRGB8
toPixelRGB8 (r, g, b) = PixelRGB8 r g b

testImage :: (Monad m)=> Gomes m (Image PixelRGB8)
testImage = do
  let nullColor = (255,255,255)
  vs <- genParentGrainBitmap nullColor (getCubicIPFColor . parentOrientation . snd)

  shape <- getVoxBoxRange
  let
    (lx, ux, ly, uy, lz, uz) = getBoxLinRange shape
    genBit x y = let
      color = vs U.! (shape %@ (VoxelPos (lx + x) (ly + y) lz))
      in toPixelRGB8 color
  return $ generateImage genBit (ux - lx) (uy - ly)

renderImage :: (Monad m)=> Gomes m BSL.ByteString
renderImage = encodePng <$> testImage

-- ====================================== Plotting VTK ===================================================

genVoxBoxAttr :: (U.Unbox a, RenderElemVTK b)=> String -> (a -> b) -> VoxBox a -> VTKAttrPoint c
genVoxBoxAttr name func qBox = mkPointAttr name (func . (grainID qBox U.!))

genProductVTKAttr :: (Monad m)=> (RenderElemVTK a, U.Unbox a, RenderElemVTK b)=> a -> ((Int, ProductGrain) -> a) -> String -> Gomes m (VTKAttrPoint b)
genProductVTKAttr nul func name = (mkPointAttr name . (U.!)) <$> genProductGrainBitmap nul func

genProductFitVTKAttr :: (Monad m)=> (RenderElemVTK a, U.Unbox a, RenderElemVTK b)=> a -> ((Quaternion, PhaseID) -> ParentGrain -> ParentProductFit -> a) -> String -> Gomes m (VTKAttrPoint b)
genProductFitVTKAttr nul func name = (mkPointAttr name . (U.!)) <$> genParentProductFitBitmap nul func

genParentVTKAttr :: (Monad m)=> (RenderElemVTK a, U.Unbox a, RenderElemVTK b)=> a -> ((Int, ParentGrain) -> a) -> String -> Gomes m (VTKAttrPoint b)
genParentVTKAttr nul f name = func <$> genParentGrainBitmap nul f
  where func = mkPointAttr name . (U.!)

getCubicIPFColor :: (Rot q)=> q -> (Word8, Word8, Word8)
getCubicIPFColor = let
  unColor (RGBColor rgb) = rgb
  in unColor . getRGBColor . snd . getIPFColor Cubic ND . convert

plotResults :: (MonadIO m, Monad m)=> FilePath -> Gomes m ()
plotResults base_file = do
  plotVTK  base_file
  plotEBSD base_file

plotEBSD :: (MonadIO m, Monad m)=> FilePath -> Gomes m ()
plotEBSD base_filename = do
  Cfg{..} <- fmap inputCfg ask
  ebsd <- genParentEBSD
  liftIO $ do
    when outputANGMap $ renderANGFile (base_filename <.> "ang") $ either id toANG ebsd
    when outputCTFMap $ renderCTFFile (base_filename <.> "ctf") $ either toCTF id ebsd

plotVTK :: (MonadIO m, Monad m)=> String -> Gomes m ()
plotVTK base_file = do
  cfg@GomesConfig{..} <- ask
  attrAvgAID <- genProductVTKAttr    (-1) fst "Product Grain ID"
  attrGID    <- genParentVTKAttr     (-1) fst "Parent Grain ID"

  attrAvgErr <- genParentVTKAttr     (-1) (unDeg . avgError . parentAvgErrorFit . snd) "Parent Avg Error Fit [deg]"
  attrDevErr <- genParentVTKAttr     (-1) (unDeg . devError . parentAvgErrorFit . snd) "Parent StdDev Error Fit [deg]"
  attrMaxErr <- genParentVTKAttr     (-1) (unDeg . maxError . parentAvgErrorFit . snd) "Parent Max Error Fit [deg]"
  attrNumPro <- genParentVTKAttr     (-1) (length . productMembers . snd)              "Parent Number of Product Components"

  attrMID    <- genProductFitVTKAttr (-1) (const $ const areaFraction)          "Product Grain Area Fraction"
  attrORVar  <- genProductFitVTKAttr (-1) (const $ const (fst . variantNumber)) "Product OR Variant Number"
  attrLocErr <- genProductFitVTKAttr (-1) (const $ const (unDeg . misfitAngle)) "Product Error Fit [deg]"

  attrAvgGIPF <- genParentVTKAttr     (255,255,255) (getCubicIPFColor . parentOrientation . snd)                 "Parent Orientation [IPF]"
  attrAvgAIPF <- genProductVTKAttr    (255,255,255) (getCubicIPFColor . productAvgOrientation . snd)             "Product Orientation [IPF]"
  attrGIPF    <- genProductFitVTKAttr (255,255,255) (\a b _ -> getCubicIPFColor $ getTransformedProduct cfg a b) "Product Parent Orientation Component [IPF]"
  let
    attrVoxAIPF  = genVoxBoxAttr "Voxel Product Orientation [IPF]" (getCubicIPFColor . fst) orientationBox
    attrVoxPhase = genVoxBoxAttr "Voxel Phase ID" snd orientationBox
    vtkD  = renderVoxBoxVTK grainIDBox attrs
    attrs = [ attrAvgGIPF
            , attrGIPF
            , attrGID
            , attrMID
            , attrAvgErr
            , attrDevErr
            , attrMaxErr
            , attrNumPro
            , attrLocErr
            , attrORVar
            , attrVoxPhase
            , attrVoxAIPF
            , attrAvgAIPF
            , attrAvgAID
            ]
  liftIO $ writeUniVTKfile (base_file <.> "vtr") True vtkD

-- ====================================== Plotting EBSD/CTF =============================================

genParentEBSD :: (Monad m)=> Gomes m (Either ANGdata CTFdata)
genParentEBSD = do
  pp <- asks (parentPhaseID . inputCfg)
  qs <- U.convert <$> genParentGrainBitmap mempty (parentOrientation . snd)
  ebsd <- asks inputEBSD
  return $ case ebsd of
    ANG ang -> Left  . genParentANG qs pp $ ang
    CTF ctf -> Right . genParentCTF qs pp $ ctf
  where
    genParentANG :: V.Vector Quaternion -> Maybe PhaseID -> ANGdata -> ANGdata
    genParentANG qs parentPhase ang = ang {A.nodes = V.zipWith insRotation qs (A.nodes ang)}
      where
        insRotation q angPoint = angPoint {
          A.rotation = q,
          A.phaseNum = maybe (A.phaseNum angPoint) phaseId parentPhase
          }

    genParentCTF :: V.Vector Quaternion -> Maybe PhaseID -> CTFdata -> CTFdata
    genParentCTF qs parentPhase ang = ang {C.nodes = V.zipWith insRotation qs (C.nodes ang)}
      where
        insRotation q ctfPoint = ctfPoint {
          C.rotation = q,
          C.phase = maybe (C.phase ctfPoint) phaseId parentPhase
          }

-- ========================================== Debugging ==========================================

renderQuaternions ::Cfg -> FilePath -> Vector Quaternion -> IO ()
renderQuaternions Cfg{..} base_file qs = let
  vtk  = renderSO3PointsVTK . U.map quaternionToSO3 $ qs
  attr = mkPointValueAttr "IPF" (\i _ -> getCubicIPFColor $ qs U.! i)
  in writeUniVTKfile (base_file <.> "vtu") True (vtk `addPointValueAttr` attr)

_plotOrientations :: (MonadIO m, Monad m)=> FilePath -> Int -> Gomes m ()
_plotOrientations base_file gid = do
  GomesConfig{..} <- ask
  GomesState{..}  <- get
  let mProductGrain = HM.lookup gid productGrains
      mParentGrain  = V.find (elem gid . productMembers) parentGrains
      mFit          = mParentGrain >>= fmap snd . L.find ((== gid) . fst) . uncurry zip . (productMembers &&& parentErrorPerProduct)
      getProductOrientations = V.map getQ . productVoxelPos
      getQ = fst . (grainID orientationBox U.!)
  forM_ ((,,) <$> mProductGrain <*> mParentGrain <*> mFit)  $ \(productGrain, parentGrain, _fit) -> do
    let qs = V.convert . V.map (qFZ . getQinFZ) . getProductOrientations $ productGrain
    liftIO $ do
      renderQuaternions inputCfg (base_file <> "_product") qs
      renderQuaternions inputCfg (base_file <> "_parent" ) $ U.fromList [parentOrientation parentGrain]
