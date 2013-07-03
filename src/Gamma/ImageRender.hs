{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
module Gamma.ImageRender where

import qualified Data.Vector as V
import qualified Data.List   as L

import           Hammer.Math.Algebra
import           Hammer.Reader.ANGReader
import           Hammer.Render.VTK.VTKRender

data Rendable = forall a . RenderAttr a => Rendable (String, EBSDpoint -> a)

mkRedable :: (RenderAttr a)=> String -> (EBSDpoint -> a) -> Rendable
mkRedable name f = Rendable (name, f)

showQI    = mkRedable "QI" qi
showCI    = mkRedable "CI" ci
showPhase = mkRedable "Phase" phase
showOrien = mkRedable "Orientation" (mkVec3 . rotation)

-- data DataFunc attr = DataFunc (String, EBSDpoint -> attr)

renderANG :: [Rendable] -> EBSDdata -> VTK Double
renderANG fs ed@EBSDdata{..}
  | not hexGrid && col_even == col_odd = vtk
  | hexGrid                            = renderANG fs $ toSqrGrid ed
  | otherwise = error "[ImageRender] Improper square grid ANG file."
  where
    EBSDinfo{..} = headInfo
    Gridinfo{..} = grid
    (row, col_even, col_odd) = rowCols
    (stepX, stepY)           = xystep
    vtkBase = mkSPVTK "EBSD map" (row, col_even, 1) (xstart, ystart, zstart) (stepX, stepY, 1)
    vtk     = L.foldl' addData vtkBase fs
    addData vtk (Rendable (name, f)) = let
      func i _ = f $ nodes V.! i
      in addDataPoints vtk (mkPointAttr name func)

toSqrGrid = undefined