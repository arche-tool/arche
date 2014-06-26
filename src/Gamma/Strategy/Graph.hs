{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}

module Gamma.Strategy.Graph
       ( run
       , Cfg(..)
       ) where

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

data Cfg =
  Cfg
  { misoAngle   :: Deg
  , ang_input   :: FilePath
  , base_output :: FilePath
  } deriving (Show)

run :: Cfg -> IO ()
run Cfg{..} = do
  ang <- parseANG ang_input
  vbq <- case ebsdToVoxBox ang rotation of
    Right x -> return x
    Left s  -> error s
  case getGrainID misoAngle Cubic vbq of
    Nothing -> print "No grain detected!"
    Just vg -> let
      (micro, gids) = getMicroVoxel $ resetGrainIDs vg
      attrs = [mkPointAttr "GrainID" (unGrainID . ((grainID gids) U.!))]
      in do
        writeUniVTKfile (base_output ++ ".vtr")        True $ renderVoxBoxVTK      gids attrs
        writeUniVTKfile (base_output ++ "_faces.vtu")  True $ renderMicroFacesVTK  gids micro
        writeUniVTKfile (base_output ++ "_edges.vtu")  True $ renderMicroEdgesVTK  gids micro
        writeUniVTKfile (base_output ++ "_vertex.vtu") True $ renderMicroVertexVTK gids micro
