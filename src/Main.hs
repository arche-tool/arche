{-# LANGUAGE RecordWildCards #-}
module Main where

import           File.ANGReader              (parseANG)
import           Hammer.Render.VTK.VTKRender (writeUniVTKfile)
import           Texture.Orientation         (RefFrame (..))
import           Texture.Symmetry            (Symm (..))
import           System.Directory            (doesFileExist)

import           Options.Applicative
import           System.FilePath

import           Gamma.OMRender
import           Gamma.GBRender
import           Gamma.Grains

data Gammafier =
  Gammafier
  { ang_input  :: String
  , vtk_output :: Maybe String
  , tolerance  :: Maybe Double
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
  <*> (optional . option)
      (  long "error-tolerance"
      <> short 'e'
      <> metavar "error[%]"
      <> help "The default error is 5%.")

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
      then renderTest ang_input outName
      else putStrLn "Invalid input file. Try agian!"

renderTest :: FilePath -> FilePath -> IO ()
renderTest fin fout = do
  ang <- parseANG fin
  let
    vboxQ  = getVoxBox ang
    viewGB = [ showGBMiso   Cubic
             , showGBMisoKS Cubic
             ]
    viewOM = [ showOMQI
             , showOMCI
             , showOMPhase
             , showOMIPF    Cubic ND
             , showGrainIDs Cubic ang
             ]
    vtkGB = renderGB viewGB vboxQ
    vtkOM = renderOM viewOM ang
  writeUniVTKfile (fout <.> "vti") True vtkOM
  writeUniVTKfile (fout <.> "vtu") True vtkGB
