{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import           System.Directory            (doesFileExist)
import           Control.DeepSeq
import           Control.Parallel.Strategies

import           Options.Applicative
import           System.FilePath

import           Texture.Orientation
import           Hammer.VoxBox.Base
import           Hammer.Render.VTK.VTKRender (writeUniVTKfile)
import           Hammer.Math.Algebra         (Vec3(..), Vec4(..))
import           Hammer.MicroGraph           (mkGrainID)
import           File.ANGReader              (parseANG, rotation, nodes)
import           Texture.Symmetry            (Symm (..), toFZ)

import           Gamma.OMRender
import           Gamma.GBRender
import           Gamma.Grains
import           Gamma.GammaFinder
import           Gamma.KurdjumovSachs

import    Debug.Trace

data Gammafier =
  Gammafier
  { ang_input  :: String
  , vtk_output :: Maybe String
  , grain_miso :: Deg
  , tolerance  :: Double
  } deriving (Show)

gammafier :: Parser Gammafier
gammafier = Gammafier
  <$> strOption
      (  long "input"
      <> short 'i'
      <> metavar "ANG_IN"
      <> help "Input file (.ang) target for correction." )
  <*> (optional . strOption)
      (  long "output"
      <> short 'o'
      <> metavar "VTK_OUT"
      <> help "VTK visualization.")
  <*> ((Deg . abs) <$> option
      (  long "grain-miso"
      <> short 'm'
      <> metavar "deg[o]"
      <> value 15
      <> help "The default error is 15 deg."))
  <*> (abs <$> option
      (  long "error-tolerance"
      <> short 'e'
      <> metavar "error[%]"
      <> value 0.05
      <> help "The default error is 5%."))

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> gammafier)
      ( fullDesc
      <> progDesc "Reconstructs gamma phase from EBSD data"
      <> header "Gamma Builder" )

run :: Gammafier -> IO ()
run Gammafier{..} = let
  stdOutName = dropExtensions ang_input
  outName    = case vtk_output of
      Just x
        | isValid x -> dropExtensions x
        | otherwise -> stdOutName
      _ -> stdOutName
  in do
    inOK <- doesFileExist ang_input
    if inOK
      --then renderTest grain_miso ang_input outName
      then mkMia ang_input outName
      else putStrLn "Invalid input file. Try agian!"

renderTest :: Deg -> FilePath -> FilePath -> IO ()
renderTest miso fin fout = do
  ang <- parseANG fin
  case getGrainID miso Cubic ang of
    Nothing            -> print "No grain detected!"
    Just (gids, gtree) -> let
      vboxQ  = getVoxBox ang
      viewGB = [ showGBMiso   Cubic
               , showGBMisoKS Cubic
               ]
      viewOM = [ showOMQI
               , showOMCI
               , showOMPhase
               , showOMIPF    Cubic ND
               , showGrainIDs gids
               ]
      (vecQ, vecGID) = getOriGID ang gids
      vtkGB  = renderGB viewGB vboxQ
      vtkOM  = renderOM viewOM ang
      vtkSO2 = renderSO2Points Cubic ND (Vec3 1 0 0) vecGID vecQ
      vtkSO3 = renderSO3Points Cubic ND vecGID vecQ
      in do
        print (U.length vecQ, U.length vecGID)
        writeUniVTKfile (fout <.> "vti") True vtkOM
        writeUniVTKfile (fout <.> "vtu") True vtkGB
        writeUniVTKfile (fout <.> "SO2" <.> "vtu") True vtkSO2
        writeUniVTKfile (fout <.> "SO3" <.> "vtu") True vtkSO3
        let
          (g, t)   = getGammaOR2 ang
          --g        = getGamma ang
          ggid     = U.singleton $ mkGrainID (-1)
          as       = U.map (toFZ Cubic . (g #<=)) (V.convert ksTrans)
          agid     = U.replicate (U.length as) (mkGrainID $ -1)
          vtkSO2_g = renderSO2Points Cubic ND (Vec3 1 0 0) ggid (U.singleton g)
          vtkSO3_g = renderSO3Points Cubic ND              ggid (U.singleton g)
          vtkSO3_a = renderSO3Points Cubic ND              agid as
        print (g, t)
        --writeUniVTKfile (fout <.> "SO2-gamma" <.> "vtu") True vtkSO2_g
        writeUniVTKfile (fout <.> "SO3-gamma" <.> "vtu") True vtkSO3_g
        writeUniVTKfile (fout <.> "SO3-alpha" <.> "vtu") True vtkSO3_a


-- | Simlpe reconstruction strategy were the orientation map is divided in non-overlapping
-- areas and the parent phase is calculated from all product orientation within the subarea.
-- The calculation is done by function minimization with pure KS.   
mkMia :: FilePath -> FilePath -> IO ()
mkMia fin fout = do
  ang <- parseANG fin
  let
    vb     = getVoxBox ang
    vb'    = runMia 100 vb
    node'  = V.zipWith (\p q -> p {rotation = q}) (nodes ang) (U.convert $ grainID vb')
    ang'   = ang {nodes = node'}
    viewOM = [ showOMQI
             , showOMCI
             , showOMPhase
             , showOMIPF    Cubic ND
             ]
    vtkOM  = renderOM viewOM ang'
  writeUniVTKfile (fout <.> "vti") True vtkOM

runMia :: Int -> VoxBox Quaternion -> VoxBox Quaternion
runMia n vb@VoxBox{..} = vb {grainID = newVec}
  where
    stg = parListChunk 5000 (evalTuple2 rpar r0)
    l = U.length grainID
    newVec = U.create $ do
      v <- UM.new l
      let qis  = divConq dimension
          qisp = qis `using` stg
      mapM_ (save v) qisp
      return v
    save v (ga, is) = U.mapM_ (\i -> UM.write v i ga) is
    getG br = let
      ps = V.fromList $ getRangePos br
      is = V.convert $ V.map (dimension %@) ps
      qs = U.map (grainID U.!) is
      in (findGamma ksORs qs, is)
    divConq br
      | sizeVoxBoxRange br <= 0 = []
      | sizeVoxBoxRange br <= n = [getG br]
      | otherwise = case splitInTwoBox br of
        Just (br1, br2) -> divConq br1 ++ divConq br2
        Nothing         -> []

instance NFData Quaternion where
  rnf = rnf . quaterVec

instance NFData Vec4 where
  rnf (Vec4 a b c d) = a `seq` b `seq` c `seq` d `seq` ()
