{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}

module Arche.Strategy.Graph
       ( run
       , processEBSD
       , Cfg(..)
       ) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector.Unboxed  as U
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

data Cfg =
  Cfg
  { misoAngle   :: Deg
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

processEBSD :: Cfg -> BSL.ByteString -> Either String (VoxBox GrainID, MicroVoxel, [VTKAttrPoint a])
processEBSD Cfg{..} bs = do
  ebsd <- loadEBSD bs
  vbp <- readEBSDToVoxBox C.phase    A.phaseNum ebsd
  vbq <- readEBSDToVoxBox C.rotation A.rotation ebsd
  case getGrainID misoAngle Cubic (vbq) of
    Nothing -> Left "No grain detected!"
    Just vg -> let
      (micro, gids) = getMicroVoxel $ resetGrainIDs vg
      attrGID   = genVoxBoxAttr "GrainID" unGrainID gids
      attrIPF   = genVoxBoxAttr "IPF"   getCubicIPFColor vbq
      attrPhase = genVoxBoxAttr "Phase" id vbp
      attrs = [attrGID, attrIPF, attrPhase]
      in Right (gids, micro, attrs)
