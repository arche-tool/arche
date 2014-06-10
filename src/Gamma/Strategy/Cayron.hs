{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}

module Gamma.Strategy.Cayron
       ( run ) where

import qualified Data.Vector                  as V
import qualified Data.Vector.Unboxed          as U
import qualified Data.Vector.Unboxed.Mutable  as MU
import qualified Data.HashMap.Strict          as HM
import qualified Data.HashSet                 as HS

import           Data.HashMap.Strict (HashMap)
import           Data.HashSet        (HashSet)
import           Data.Maybe          (mapMaybe)

import           System.FilePath
import           Control.Parallel.Strategies

import           Hammer.Math.Algebra         (Vec3(..))
import           Hammer.VoxBox
import           Hammer.VTK.VoxBox
import           Hammer.VTK
import           Hammer.Graph
import           Hammer.MicroGraph

import           Texture.Symmetry            (Symm (..))
import           Texture.Orientation
import           File.ANGReader

import           Gamma.Grains
import           Gamma.OR

import Debug.Trace
dbg a = trace (show a) a
dbgs s a = trace (show s ++ " <=> " ++ show a) a

run :: Deg -> FilePath -> FilePath -> IO ()
run miso fin fout = do
  ang <- parseANG fin
  let vbq = ebsdToVoxBox ang rotation
  case getGrainID miso Cubic vbq of
    Nothing               -> print "No grain detected!"
    Just gidMap@(vbgid, gtree) -> let
      mkr = fst $ getMicroVoxel gidMap
      fs  = findConnFaces vbq gtree mkr
      vtk = renderGB vbq mkr fs
      ggh = grainsGraph fs
      vbgid2 = gammaBox gidMap ggh
      vtk2 = renderVoxBoxVTK vbgid2 [cell1, cell2]
      cell1 = mkCellAttr "GammaGB" (\i _ _ -> (grainID vbgid2) U.! i)
      cell2 = mkCellAttr "AlphaGB" (\i _ _ -> unGrainID $ (grainID vbgid)  U.! i)
      in do
        print $ grainsGraph fs
        writeUniVTKfile (fout <.> "vtu") True vtk
        writeUniVTKfile (fout <.> "vtr") True vtk2

gammaBox :: (VoxBox GrainID, HashMap Int (V.Vector VoxelPos)) -> [HashSet Int] -> VoxBox Int
gammaBox (vb@VoxBox{..}, gidMap) gs = let
  getIS oldID = maybe (V.empty) (V.map (dimension %@)) (HM.lookup oldID gidMap)
  func v (newID, s) = mapM_ (V.mapM_ (\i -> MU.write v i newID) . getIS) (HS.toList s)
  vec = U.create $ do
    v <- MU.replicate (U.length grainID) (-1)
    mapM_ (func v) (zip [1..] gs)
    return v
  in vb {grainID = vec}

grainsGraph :: [FaceID] -> [HashSet Int]
grainsGraph = connComp . mkUniGraph [] . map ((\x -> (x, 1.0 :: Double)) . unFaceID)

findConnFaces :: VoxBox Quaternion -> HashMap Int (V.Vector VoxelPos) -> MicroVoxel -> [FaceID]
findConnFaces vbq gmap micro = let
  es   = HM.keys $ microEdges micro
  qMap = HM.map (shitQAvg . V.convert . V.map (vbq #!)) gmap
  fs   =  (map (testEdge qMap) es) `using` parListChunk 100 rpar
  --stg  = parListChunk 10 (rdeepseq)
  in concatMap (\(f1, f2, f3)-> [f1, f2, f3]) (mapMaybe id fs) -- (fs `using` stg)

testEdge :: HashMap Int Quaternion -> EdgeID -> Maybe (FaceID, FaceID, FaceID)
testEdge qMap eid = case unEdgeID eid of
  Left (f1, f2, f3)
    | testFace qMap f1 &&
      testFace qMap f2 &&
      testFace qMap f3 -> return (f1, f2, f3)
  _ -> Nothing

testFace :: HashMap Int Quaternion -> FaceID -> Bool
testFace grains fid = let
  (g1, g2) = unFaceID fid
  test = do
   q1 <- HM.lookup g1 grains
   q2 <- HM.lookup g2 grains
   return $ hasOR q1 q2
  in dbgs (g1, g2) $ maybe False id test

hasOR :: Quaternion -> Quaternion -> Bool
hasOR q1 q2 = (fromAngle $ Deg 7.5) > misoKS Cubic q1 q2

renderGB :: VoxBox Quaternion -> MicroVoxel -> [FaceID] -> VTK Vec3
renderGB vb micro fs = addData $ renderAllElemProp vb fprop
  where
    fprop = mapMaybe ((flip getFaceProp) micro) fs
    addData vtk = let
      func _ _ _ = 1 :: Int
      in addDataCells vtk (mkCellAttr "ConnGB" func)
