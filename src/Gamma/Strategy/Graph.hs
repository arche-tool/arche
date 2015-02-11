{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}

module Gamma.Strategy.Graph
       ( run
       , Cfg(..)
       ) where

import qualified Data.Vector.Unboxed as U

import           Data.Word

import           Hammer.VoxBox
import           Hammer.VoxConn
import           Hammer.VTK.VoxBox
import           Hammer.VTK
import           Hammer.MicroGraph

import           File.ANGReader
import           Texture.Symmetry
import           Texture.Orientation
import           Texture.IPF

import           Gamma.Grains
import           Gamma.OMRender

data Cfg =
  Cfg
  { misoAngle   :: Deg
  , ang_input   :: FilePath
  , base_output :: FilePath
  } deriving (Show)


genVoxBoxAttr :: (U.Unbox a, RenderElemVTK b)=>
                 String -> (a -> b) -> VoxBox a -> VTKAttrPoint c
genVoxBoxAttr name func qBox = mkPointAttr name (func . ((grainID qBox) U.!))

getCubicIPFColor :: (Rot q)=> q -> (Word8, Word8, Word8)
getCubicIPFColor = let
  unColor (RGBColor rgb) = rgb
  in unColor . getRGBColor . snd . getIPFColor Cubic ND . toQuaternion

run :: Cfg -> IO ()
run Cfg{..} = do
  ang <- parseANG ang_input
  vbp <- case angToVoxBox ang phaseNum of
    Right x -> return x
    Left s  -> error s
  vbq <- case angToVoxBox ang rotation of
    Right x -> return x
    Left s  -> error s
  case getGrainID misoAngle Cubic (vbq) of
    Nothing -> print "No grain detected!"
    Just vg -> let
      (micro, gids) = getMicroVoxel $ resetGrainIDs vg
      attrGID   = genVoxBoxAttr "GrainID" unGrainID gids
      attrIPF   = genVoxBoxAttr "IPF"   getCubicIPFColor vbq
      attrPhase = genVoxBoxAttr "Phase" id vbp
      attrs = [attrGID, attrIPF, attrPhase]
      in do
        writeUniVTKfile (base_output ++ ".vtr")        True $ renderVoxBoxVTK      gids attrs
        writeUniVTKfile (base_output ++ "_faces.vtu")  True $ renderMicroFacesVTK  gids micro
        writeUniVTKfile (base_output ++ "_edges.vtu")  True $ renderMicroEdgesVTK  gids micro
        writeUniVTKfile (base_output ++ "_vertex.vtu") True $ renderMicroVertexVTK gids micro
