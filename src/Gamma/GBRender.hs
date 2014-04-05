{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}

module Gamma.GBRender
       ( RenderGB
       , showGBMiso
       , showGBMisoKS
       , renderGB
       ) where

import qualified Data.List   as L
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import           Hammer.Math.Algebra
import           Hammer.Render.VTK.VTKRender
import           Texture.Orientation
import           Texture.Symmetry
import           Hammer.VoxBox.Base

import           Gamma.KurdjumovSachs

-- =======================================================================================

data RenderGB = forall a . RenderElemVTK a => RenderGB String (Quaternion -> Quaternion -> a)

showGBMiso :: Symm -> RenderGB
showGBMiso symm = RenderGB "Misorientation" (\q1 q2 -> unDeg $ toAngle $ (getMisoAngle symm q1 q2))

showGBMisoKS :: Symm -> RenderGB
showGBMisoKS symm = RenderGB "Misorientation KS" (\q1 q2 -> unDeg $ toAngle $ (misoKS symm q1 q2))

-- =======================================================================================

extendBoxFace :: VoxBox a -> VoxBox a
extendBoxFace VoxBox{..} = VoxBox d o spacing (V.empty)
  where
    VoxBoxDim nx ny nz    = vbrDim dimension
    VoxBoxOrigin x0 y0 z0 = origin
    VoxelDim dx dy dz     = spacing
    d = dimension {vbrDim = VoxBoxDim (nx+1) (ny+1) (nz+1)}
    o = VoxBoxOrigin (x0-dx) (y0-dy) (z0-dz)

renderGB :: [RenderGB] -> VoxBox Quaternion -> VTK Vec3
renderGB fs box = L.foldl' addData vtkBase fs
  where
    ps       = getRangePos range
    range    = dimension box
    extRange = dimension extBox
    extBox   = extendBoxFace box
    foo acc a = gb XDir a $ gb YDir a $ gb ZDir a acc
    (values, cells) = unzip $ L.foldl' foo [] ps
    gb dir a acc
      | dir == XDir = func xDir yDir deltaYZplus zDir
      | dir == YDir = func yDir zDir deltaXZplus xDir
      | otherwise   = func zDir xDir deltaXYplus yDir
      where
        func dir0 dir1 dir2 dir3 = let
          b   = a #+# dir0
          ib  = extRange %@ b
          ib1 = extRange %@ (b #+# dir1)
          ib2 = extRange %@ (b #+# dir2)
          ib3 = extRange %@ (b #+# dir3)
          in if isInBox range b
             then ((box #! a, box #! b), (ib, ib1, ib2, ib3)) : acc
             else acc
    points = U.generate (sizeVoxBoxRange extRange) (evalVoxelPos extBox . (extRange %#))
    vtkBase = mkUGVTK "GB map" points (U.fromList cells)
    addData vtk (RenderGB name f) = let
      func i _ _ = (uncurry f) $ (V.fromList values) V.! i
      in addDataCells vtk (mkCellAttr name func)

instance RenderCell (Int, Int, Int, Int) where
  makeCell (a,b,c,d) = U.fromList [a,b,c,d]
  getType _          = VTK_QUAD
