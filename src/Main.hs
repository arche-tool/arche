{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Gamma.Strategy.Graph    as Graph
import qualified Gamma.Strategy.ORFit    as ORFit
import qualified Gamma.Strategy.Cayron   as Cayron
import qualified Gamma.Strategy.Miyamoto as Miyamoto

import           System.Directory            (doesFileExist)

import           Options.Applicative
import           System.FilePath

import           Texture.Orientation (Deg(..))

data RunMode
  = ShowGraph
  | ORFit
  | Miyamoto
  | Cayron
  deriving (Show, Eq)

data Gammafier =
  Gammafier
  { ang_input  :: String
  , vtk_output :: Maybe String
  , grain_miso :: Deg
  , tolerance  :: Double
  , runMode    :: RunMode
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
  <*> parseMode

parseMode :: Parser RunMode
parseMode = subparser
 ( command "graph"
   (info (pure ShowGraph)
   (progDesc "Render grain's ID, vertexes, edges and faces."))
 <> command "orfit"
   (info (pure ORFit)
   (progDesc "Fit OR on all points where ci > 0.1"))
 <> command "myiamoto"
   (info (pure Miyamoto)
   (progDesc "Reconstruction based on Myiamoto's method."))
 <> command "cayron"
   (info (pure Cayron)
   (progDesc "Reconstruction based on Cayron's method."))
 )

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
    if not inOK
      then putStrLn "Invalid input file. Try agian!"
      else case runMode of
      Cayron   -> Cayron.run grain_miso ang_input outName
      Miyamoto -> Miyamoto.run          ang_input outName
      ORFit    -> ORFit.run grain_miso  ang_input outName
      _        -> Graph.run grain_miso  ang_input outName
