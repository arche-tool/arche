{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

module Gamma.ImageRender
       ( RenderPropEBSD
       , showEBSDQI
       , showEBSDCI
       , showEBSDPhase
       , showEBSDIPF
       , showGrainIDs
       , renderANG
       ) where

import qualified Data.List   as L
import qualified Data.Vector as V

import           Control.Applicative ((<$>))
import           Hammer.MicroGraph   (GrainID, unGrainID)

import           Hammer.Reader.ANGReader
import           Hammer.Render.VTK.VTKRender
import           Hammer.Texture.IPF
import           Hammer.Texture.Orientation
import           Hammer.Texture.Symmetry
import           Hammer.VoxBox.Base
import           Hammer.VoxBox.VoxConnFinder

-- =======================================================================================

data RenderPropEBSD = forall a . RenderElemVTK a => RenderPropEBSD String (EBSDpoint -> a)
                    | forall a . RenderElemVTK a => RenderAlterMap String (Int -> a)

showEBSDQI :: RenderPropEBSD
showEBSDQI = RenderPropEBSD "QI" qi

showEBSDCI :: RenderPropEBSD
showEBSDCI = RenderPropEBSD "CI" ci

showEBSDPhase :: RenderPropEBSD
showEBSDPhase = RenderPropEBSD "Phase" phase

showEBSDIPF :: Symm -> RefFrame -> RenderPropEBSD
showEBSDIPF symm ref = let
  unColor (RGBColor rgb) = rgb
  foo = unColor . getRGBColor . snd . getIPFColor symm ref . rotation
  in RenderPropEBSD ("IPF " ++ show ref) foo

showGrainIDs :: Symm -> EBSDdata -> RenderPropEBSD
showGrainIDs symm ed = let
  foo = maybe (const 0) (\m -> unGrainID . ((grainID m) V.!)) (getGrainID2D symm ed)
  in  RenderAlterMap "GrainID" foo

-- =======================================================================================

checkSqrANG :: EBSDdata -> Bool
checkSqrANG EBSDdata{..} = let
  Gridinfo{..}       = grid
  (row, cEven, cOdd) = rowCols
  in    not hexGrid
     && cEven          == cOdd
     && V.length nodes == (row * cEven)

renderANG :: [RenderPropEBSD] -> EBSDdata -> VTK Double
renderANG fs ed@EBSDdata{..}
  | checkSqrANG ed = L.foldl' addData vtkBase fs
  | otherwise      = error "[ImageRender] Improper square grid ANG file."
  where
    EBSDinfo{..}    = headInfo
    Gridinfo{..}    = grid
    (row, cEven, _) = rowCols
    (stepX, stepY)  = xystep
    vtkBase = mkSPVTK "EBSD map"
              (cEven , row   , 1     )
              (xstart, ystart, zstart)
              (stepX , stepY , 1     )
    addData vtk (RenderPropEBSD name f) = let
      func i _ = f $ nodes V.! i
      in addDataPoints vtk (mkPointAttr name func)
    addData vtk (RenderAlterMap name f) = let
      func i _ = f i
      in addDataPoints vtk (mkPointAttr name func)

getGrainID2D :: Symm -> EBSDdata -> Maybe (VoxBox GrainID)
getGrainID2D symm ed = let
  isGrain a b = let
    omega = getMisoAngle symm a b
    in (fromAngle $ Deg 15) > omega
  vbox     = getVoxBox2D ed
  vboxSymm = vbox { grainID = V.map (toFZ symm) (grainID vbox) }
  in fst . resetGrainIDs <$> grainFinder isGrain vboxSymm

getVoxBox2D :: EBSDdata -> VoxBox Quaternion
getVoxBox2D EBSDdata{..} = let
  EBSDinfo{..}       = headInfo
  Gridinfo{..}       = grid
  (xstep, ystep)     = xystep
  (row, col_even, _) = rowCols
  boxorg = VoxelPos 1 1 1
  boxdim = VoxBoxDim col_even row 1
  dime   = VoxBoxRange boxorg boxdim
  org    = VoxBoxOrigin xstart ystart zstart
  spc    = VoxelDim xstep ystep ((xstep + ystep) / 2)
  in VoxBox dime org spc (V.map rotation nodes)
