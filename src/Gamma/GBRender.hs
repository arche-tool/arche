{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}

module Gamma.GBRender
       ( RenderGB
       , showGBMiso
       , showGBMisoKS
       , renderGB
       ) where

import qualified Data.List           as L
import qualified Data.Vector.Unboxed as U

import           Hammer.Math.Algebra
import           Hammer.VoxBox
import           Hammer.VTK
import           Hammer.VTK.VoxBox ()
import           Texture.Orientation
import           Texture.Symmetry

import           Gamma.OR

-- =======================================================================================

data RenderGB = forall a . RenderElemVTK a => RenderGB String (Quaternion -> Quaternion -> a)

showGBMiso :: Symm -> RenderGB
showGBMiso symm = RenderGB "Misorientation" (\q1 q2 -> unDeg $ toAngle $ (getMisoAngle symm q1 q2))

showGBMisoKS :: Symm -> RenderGB
showGBMisoKS symm = RenderGB "Misorientation KS" (\q1 q2 -> unDeg $ toAngle $ (misoDoubleKS symm q1 q2))

-- =======================================================================================

extendBoxFace :: (U.Unbox a)=> VoxBox a -> VoxBox a
extendBoxFace VoxBox{..} = VoxBox d o spacing (U.empty)
  where
    VoxBoxDim nx ny nz    = vbrDim dimension
    VoxBoxOrigin x0 y0 z0 = origin
    VoxelDim dx dy dz     = spacing
    d = dimension {vbrDim = VoxBoxDim (nx+1) (ny+1) (nz+1)}
    o = VoxBoxOrigin (x0-dx) (y0-dy) (z0-dz)

renderGB :: [RenderGB] -> VoxBox Quaternion -> VTK Vec3
renderGB fs box = vtk
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
    vtk    = mkUGVTK "GB map" points (U.fromList cells) [] attrs
    attrs  = map getAttr fs
    getAttr (RenderGB name f) = let
      func i _ _ = (uncurry f) $ (U.fromList values) U.! i
      in mkCellAttr name func
