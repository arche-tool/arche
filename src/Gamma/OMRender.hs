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
       ) where

import qualified Data.List   as L
import qualified Data.Vector as V

import           Hammer.MicroGraph   (unGrainID)

import           File.ANGReader
import           Hammer.Render.VTK.VTKRender
import           Texture.IPF
import           Texture.Orientation
import           Texture.Symmetry
import           Hammer.VoxBox.Base

import           Gamma.Grains

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

showGrainIDs :: Symm -> EBSDdata -> RenderOM
showGrainIDs symm ed = let
  foo = maybe (const 0) (\m -> unGrainID . ((grainID m) V.!)) (getGrainID symm ed)
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
