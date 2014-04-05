{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

module Gamma.OMRender
       ( RenderOM
       , showOMQI
       , showOMCI
       , showOMPhase
       , showOMIPF
       , showGrainIDs
       , renderOM
       , renderSO2Points
       , renderSO3Points
       , getOriGID
       ) where

import qualified Data.List   as L
import qualified Data.Vector as V

import           Data.Vector         (Vector)
import           Hammer.MicroGraph   (GrainID, unGrainID)

import           File.ANGReader
import           Hammer.Render.VTK.VTKRender
import           Hammer.Math.Algebra
import           Hammer.VoxBox.Base
import           Texture.IPF
import           Texture.Orientation
import           Texture.Symmetry
import           Texture.SH.HyperSphere

-- =======================================================================================

data RenderOM = forall a . RenderElemVTK a => RenderOM String (EBSDpoint -> a)
              | forall a . RenderElemVTK a => RenderAlterMap String (Int -> a)

showOMQI :: RenderOM
showOMQI = RenderOM "QI" qi

showOMCI :: RenderOM
showOMCI = RenderOM "CI" ci

showOMPhase :: RenderOM
showOMPhase = RenderOM "Phase" phaseNum

showOMIPF :: Symm -> RefFrame -> RenderOM
showOMIPF symm ref = let
  unColor (RGBColor rgb) = rgb
  foo = unColor . getRGBColor . snd . getIPFColor symm ref . rotation
  in RenderOM ("IPF " ++ show ref) foo

showGrainIDs :: VoxBox GrainID -> RenderOM
showGrainIDs gids = let
  foo = unGrainID . ((grainID gids) V.!)
  in  RenderAlterMap "GrainID" foo

-- =======================================================================================

checkSqrANG :: EBSDdata -> Bool
checkSqrANG EBSDdata{..} = let
  Gridinfo{..}       = grid
  (row, cEven, cOdd) = rowCols
  in not hexGrid
     && cEven          == cOdd
     && V.length nodes == (row * cEven)

renderOM :: [RenderOM] -> EBSDdata -> VTK Double
renderOM fs ed@EBSDdata{..}
  | checkSqrANG ed = L.foldl' addData vtkBase fs
  | otherwise      = error "[ImageRender] Improper square grid ANG file."
  where
    EBSDinfo{..}    = ebsdInfo
    Gridinfo{..}    = grid
    (row, cEven, _) = rowCols
    (stepX, stepY)  = xystep
    vtkBase = mkSPVTK "OM map"
              (cEven , row   , 1     )
              (xstart, ystart, zstart)
              (stepX , stepY , 1     )
    addData vtk (RenderOM name f) = let
      func i _ = f $ nodes V.! i
      in addDataPoints vtk (mkPointAttr name func)
    addData vtk (RenderAlterMap name f) = let
      func i _ = f i
      in addDataPoints vtk (mkPointAttr name func)

getOriGID :: EBSDdata -> VoxBox GrainID -> (Vector Quaternion, Vector GrainID)
getOriGID EBSDdata{..} vboxgid = let
  is   = V.findIndices ((> 0.1) . ci) nodes
  gids = V.map ((grainID vboxgid) V.!)  is
  qs   = V.map (rotation . (nodes V.!)) is
  in (qs, gids)

renderSO3Points :: Symm -> RefFrame -> Vector GrainID -> Vector Quaternion -> VTK Vec3
renderSO3Points symm ref gids qs = let
  unColor (RGBColor rgb) = rgb

  vtkBase = renderSO3PointsVTK (V.map quat qs)

  quat  = quaternionToSO3 . toFZ symm
  color = unColor . getRGBColor . snd . getIPFColor symm ref

  func1 i _    = color (qs V.! i)
  addColor vtk = addDataPoints vtk (mkPointAttr "IPF colors" func1)

  func2 i _  = unGrainID (gids V.! i)
  addGID vtk = addDataPoints vtk (mkPointAttr "GrainID" func2)

  in addGID $ addColor vtkBase

renderSO2Points :: Symm -> RefFrame -> Vec3 -> Vector GrainID -> Vector Quaternion -> VTK Vec3
renderSO2Points symm ref v gidv qs = let
  unColor (RGBColor rgb) = rgb

  getDirs  = V.singleton . cartToSO2 . activeVecRotation v
  getColor = unColor . getRGBColor . snd . getIPFColor symm ref

  func i q = let
    color = getColor q
    gid   = gidv V.! i
    dirs  = getDirs q
    n = V.length dirs
    in (dirs, V.replicate n gid, V.replicate n color)

  (ds, gs, cs) = V.unzip3 $ V.imap func qs
  ds' = V.concatMap id ds
  gs' = V.concatMap id gs
  cs' = V.concatMap id cs

  vtkBase = renderSO2PointsVTK ds'

  func1 i _    = cs' V.! i
  addColor vtk = addDataPoints vtk (mkPointAttr "IPF colors" func1)

  func2 i _  = unGrainID (gs' V.! i)
  addGID vtk = addDataPoints vtk (mkPointAttr "GrainID" func2)

  in addGID $ addColor vtkBase
