{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Gamma.Strategy.Graph       as Graph
import qualified Gamma.Strategy.ORFitSingle as ORFitSingle
import qualified Gamma.Strategy.ORFitAll    as ORFitAll
import qualified Gamma.Strategy.Cayron      as Cayron
import qualified Gamma.Strategy.Miyamoto    as Miyamoto
import qualified Gamma.Strategy.Gomes       as Gomes
import qualified Gamma.Strategy.GomesGraph  as GomesGraph

import           System.Directory            (doesFileExist)

import           Options.Applicative
import           System.FilePath

import           Texture.Orientation (Deg(..))

data RunMode
  = ShowGraph
  | ORFitAll
  | ORFitSingle
  | Miyamoto
  | Cayron
  | Gomes
  | GomesGraph
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
 <> command "orfit-single"
   (info (pure ORFitSingle)
   (progDesc "Fit OR on all points where ci > 0.1"))
 <> command "orfit-all"
   (info (pure ORFitAll)
   (progDesc "Fit OR on all grain boundaries."))
 <> command "myiamoto"
   (info (pure Miyamoto)
   (progDesc "Reconstruction based on Myiamoto's method."))
 <> command "cayron"
   (info (pure Cayron)
   (progDesc "Reconstruction based on Cayron's method."))
 <> command "gomes"
   (info (pure Gomes)
   (progDesc "Reconstruction based on Gomes's method."))
 <> command "gomes-graph"
   (info (pure GomesGraph)
   (progDesc "Reconstruction based on Gomes's method(graph clustering)."))
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
      Miyamoto    -> Miyamoto.run                ang_input outName
      Gomes       -> Gomes.run        grain_miso ang_input outName
      GomesGraph  -> GomesGraph.run   grain_miso ang_input outName
      Cayron      -> Cayron.run       grain_miso ang_input outName
      ORFitSingle -> ORFitSingle.run  grain_miso ang_input outName
      ORFitAll    -> ORFitAll.run     grain_miso ang_input outName
      _           -> Graph.run        grain_miso ang_input outName
