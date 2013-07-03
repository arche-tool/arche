{-# LANGUAGE RecordWildCards #-}
module Main where

import           System.Directory            (doesFileExist)
import           Hammer.Render.VTK.VTKRender (writeUniVTKfile)
import           Hammer.Reader.ANGReader     (parseANG)

import           Options.Applicative
import           System.FilePath

import           Gamma.ImageRender

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
  <*> parseMaybeStrOpt
      (  long "output"
      <> short 'o'
      <> metavar "VTK_OUT"
      <> help "VTK visualization.")
  <*> parseMaybeOpt
      (  long "error-tolerance"
      <> short 'e'
      <> metavar "error[%]"
      <> help "The default error is 5%.")

parseMaybeStrOpt cfg = (Just <$> strOption cfg) <|> pure Nothing
parseMaybeOpt    cfg = (Just <$>    option cfg) <|> pure Nothing

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
    view = [showQI, showCI, showPhase, showOrien]
    vtk  = renderANG view ang
  writeUniVTKfile (fout <.> "vti") vtk