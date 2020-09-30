{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module Arche.Strategy.Graph
       ( run
       , processEBSD
       , Cfg(..)
       ) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector.Unboxed  as U
import Control.Arrow ((&&&))
import Data.Word

import qualified File.ANGReader as A
import qualified File.CTFReader as C
import Hammer.VoxBox
import Hammer.VoxConn
import Hammer.VTK
import Hammer.MicroGraph
import File.EBSD
import Texture.Symmetry
import Texture.Orientation
import Texture.IPF

import Arche.Grains
import Arche.OR

data Cfg =
  Cfg
  { misoAngle   :: Deg
  , parentPhase  :: Maybe Phase
  , productPhase :: Phase
  } deriving (Show)

genVoxBoxAttr :: (U.Unbox a, RenderElemVTK b)=>
                 String -> (a -> b) -> VoxBox a -> VTKAttrPoint c
genVoxBoxAttr name func qBox = mkPointAttr name (func . ((grainID qBox) U.!))

getCubicIPFColor :: (Rot q)=> q -> (Word8, Word8, Word8)
getCubicIPFColor = let
  unColor (RGBColor rgb) = rgb
  in unColor . getRGBColor . snd . getIPFColor Cubic ND . toQuaternion

run :: Cfg -> FilePath -> FilePath -> IO ()
run cfg ebsd_file base_output = do
  bs <- BSL.readFile ebsd_file
  case processEBSD cfg bs of
    Left err -> error err
    Right (gids, micro, attrs) -> do
      writeUniVTKfile (base_output ++ ".vtr")        True $ renderVoxBoxVTK      gids attrs
      writeUniVTKfile (base_output ++ "_faces.vtp")  True $ renderMicroFacesVTK  gids micro
      writeUniVTKfile (base_output ++ "_edges.vtp")  True $ renderMicroEdgesVTK  gids micro
      writeUniVTKfile (base_output ++ "_vertex.vtp") True $ renderMicroVertexVTK gids micro

getPhaseSelector :: Cfg -> (Int -> Phase)
getPhaseSelector Cfg{..} = let
  in case parentPhase of  
    Just parent -> \ph -> if ph == phaseId parent then parent else if ph == phaseId productPhase then productPhase else Phase ph CubicPhase 
    _           -> \ph -> if ph == phaseId productPhase then productPhase else Phase ph CubicPhase 

processEBSD :: Cfg -> BSL.ByteString -> Either String (VoxBox GrainID, MicroVoxel, [VTKAttrPoint a])
processEBSD cfg@Cfg{..} bs = do
  let phaseSelector = getPhaseSelector cfg
  ebsd <- loadEBSD bs
  vbq <- readEBSDToVoxBox
    (C.rotation &&& (phaseSelector . C.phase))
    (A.rotation &&& (phaseSelector . A.phaseNum))
    ebsd
  case getGrainID misoAngle vbq of
    Nothing -> Left "No grain detected!"
    Just vg -> let
      (micro, gids) = getMicroVoxel $ resetGrainIDs vg
      attrGID   = genVoxBoxAttr "GrainID" unGrainID gids
      attrIPF   = genVoxBoxAttr "IPF"   (getCubicIPFColor .fst) vbq
      attrPhase = genVoxBoxAttr "Phase" (phaseId . snd) vbq
      attrs = [attrGID, attrIPF, attrPhase]
      in Right (gids, micro, attrs)
