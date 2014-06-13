{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}

module Gamma.Strategy.Graph
       ( run ) where

import qualified Data.Vector.Unboxed as U

import           Hammer.VoxBox
import           Hammer.VoxConn
import           Hammer.VTK.VoxBox
import           Hammer.VTK
import           Hammer.MicroGraph

import           File.ANGReader
import           Texture.Symmetry
import           Texture.Orientation

import           Gamma.Grains

run :: Deg -> FilePath -> FilePath -> IO ()
run miso fin fout = do
  ang <- parseANG fin
  let vboxQ = ebsdToVoxBox ang rotation
  case getGrainID miso Cubic vboxQ of
    Nothing -> print "No grain detected!"
    Just vg -> let
      (micro, gids) = getMicroVoxel $ resetGrainIDs vg
      attrs = [mkPointAttr "GrainID" (unGrainID . ((grainID gids) U.!))]
      in do
        writeUniVTKfile (fout ++ ".vtr")        True $ renderVoxBoxVTK      gids attrs
        writeUniVTKfile (fout ++ "_faces.vtu")  True $ renderMicroFacesVTK  gids micro
        writeUniVTKfile (fout ++ "_edges.vtu")  True $ renderMicroEdgesVTK  gids micro
        writeUniVTKfile (fout ++ "_vertex.vtu") True $ renderMicroVertexVTK gids micro
