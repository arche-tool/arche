{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import qualified Gamma.Strategy.Graph       as Graph
import qualified Gamma.Strategy.ORFitSingle as ORFitSingle
import qualified Gamma.Strategy.ORFitAll    as ORFitAll
import qualified Gamma.Strategy.Cayron      as Cayron
import qualified Gamma.Strategy.Miyamoto    as Miyamoto
import qualified Gamma.Strategy.Gomes       as Gomes
import qualified Gamma.Strategy.GomesGraph  as GomesGraph

import           System.Directory            (doesFileExist)
import           Control.Monad               (when)
import           Data.Word                   (Word8)

import           Options.Applicative
import           System.FilePath

import           Texture.Orientation         (Deg(..))

-- ===================================== Data & class ====================================

class ParserCmdLine a where
  runAlgo  :: a -> IO ()
  validate :: a -> IO (Either a String)

data RunMode = forall a . ParserCmdLine a => RunMode {config :: a}

-- ========================================= Main ========================================

parseMode :: Parser RunMode
parseMode = subparser
 ( command "graph"
   (info (RunMode <$> parseShowGraph)
   (progDesc "Render grain's ID, vertexes, edges and faces."))
 <> command "orfit-single"
   (info (RunMode <$> parseORFitSingle)
   (progDesc "Fit OR on all points where ci > 0.1"))
 <> command "orfit-all"
   (info (RunMode <$> parseORFitAll)
   (progDesc "Fit OR on all grain boundaries."))
 <> command "myiamoto"
   (info (RunMode <$> parseMiyamoto)
   (progDesc "Reconstruction based on Myiamoto's method."))
 <> command "cayron"
   (info (RunMode <$> parseCayron)
   (progDesc "Reconstruction based on Cayron's method."))
 <> command "gomes"
   (info (RunMode <$> parseGomes)
   (progDesc "Reconstruction based on Gomes's method."))
 <> command "gomes-graph"
   (info (RunMode <$> parseGomesGraph)
   (progDesc "Reconstruction based on Gomes's method(graph clustering)."))
 )

main :: IO ()
main = do
  RunMode mode <- execParser opts
  cfg <- validate mode
  case cfg of
    Left  c -> runAlgo c
    Right s -> error s
  where
    opts = info (helper <*> parseMode)
           ( fullDesc
           <> progDesc "Reconstructs gamma phase from EBSD data"
           <> header "Gamma Builder" )

-- ======================================= Common tools ==================================

parseANGInputFile :: Parser String
parseANGInputFile = strOption
   (  long "input"
   <> short 'i'
   <> metavar "ANG_IN"
   <> help "Input file (.ang) target for correction." )

parseVTKOutputFile :: Parser (Maybe String)
parseVTKOutputFile = (optional . strOption)
   (  long "output"
   <> short 'o'
   <> metavar "VTK_OUT"
   <> help "VTK visualization.")

parseMisoAngle :: Parser Deg
parseMisoAngle = ((Deg . abs) <$> option
   (  long "grain-miso"
   <> short 'm'
   <> metavar "[Deg]"
   <> value 15
   <> help "The default error is 15 deg."))

parseInOut :: Parser (FilePath, FilePath)
parseInOut = let
  func a b = (a, getStdOut a b)
  in func <$> parseANGInputFile <*> parseVTKOutputFile

getStdOut :: FilePath -> Maybe FilePath -> FilePath
getStdOut fin dout = let
 stdOutName = dropExtensions fin
 in case dout of
   Just x
     | isValid x -> dropExtensions x
     | otherwise -> stdOutName
   _ -> stdOutName

goodInputFile :: FilePath -> IO Bool
goodInputFile fin = do
  inOK <- doesFileExist fin
  when (not inOK) (putStrLn  $ "Invalid input file! " ++ fin)
  return inOK

testInputFile :: (a -> FilePath) -> a -> IO (Either a String)
testInputFile func cfg = do
    inOk <- goodInputFile (func cfg)
    return $ if inOk
      then Left cfg
      else Right "Failed to read the input file."

-- ========================================= ConnGraph ===================================

instance ParserCmdLine Graph.Cfg where
  runAlgo  = Graph.run
  validate = testInputFile Graph.ang_input

parseShowGraph :: Parser Graph.Cfg
parseShowGraph = let
  func m (fin, fout) = Graph.Cfg m fin fout
  in func
     <$> parseMisoAngle
     <*> parseInOut

-- ===================================== OR Fit Single ===================================

instance ParserCmdLine ORFitSingle.Cfg where
  runAlgo  = ORFitSingle.run
  validate = testInputFile ORFitSingle.ang_input

parseORFitSingle :: Parser ORFitSingle.Cfg
parseORFitSingle = let
  func m (fin, fout) = ORFitSingle.Cfg m fin fout
  in func
     <$> parseMisoAngle
     <*> parseInOut

-- ======================================= OR Fit All ====================================

instance ParserCmdLine ORFitAll.Cfg where
  runAlgo  = ORFitAll.run
  validate = testInputFile ORFitAll.ang_input

parseORFitAll :: Parser ORFitAll.Cfg
parseORFitAll = let
  func m (fin, fout) = ORFitAll.Cfg m fin fout
  in func
     <$> parseMisoAngle
     <*> parseInOut
     <*> parseORbyAvg

parseORbyAvg :: Parser Bool
parseORbyAvg = switch
   (  long "or-by-avg"
   <> short 'a'
   <> help "Uses average grain orientation when optimizing OR")

-- ========================================= Cayron ======================================

instance ParserCmdLine Cayron.Cfg where
  runAlgo  = Cayron.run
  validate = testInputFile Cayron.ang_input

parseCayron :: Parser Cayron.Cfg
parseCayron = let
  func m (fin, fout) = Cayron.Cfg m fin fout
  in func
     <$> parseMisoAngle
     <*> parseInOut

-- ======================================= Miyamoto ======================================

instance ParserCmdLine Miyamoto.Cfg where
  runAlgo  = Miyamoto.run
  validate = testInputFile Miyamoto.ang_input

parseMiyamoto :: Parser Miyamoto.Cfg
parseMiyamoto = let
  func b (fin, fout) = Miyamoto.Cfg b fin fout
  in func
     <$> parseBoxSize
     <*> parseInOut

parseBoxSize :: Parser Int
parseBoxSize = abs <$> option
   (  long "box-size"
   <> short 'b'
   <> metavar "Int"
   <> value 1
   <> help "Length in voxels of the scanning box. Default 1")

-- ================================== GomesGraph =========================================

instance ParserCmdLine GomesGraph.Cfg where
  runAlgo  = GomesGraph.run
  validate = testInputFile GomesGraph.ang_input

parseGomesGraph :: Parser GomesGraph.Cfg
parseGomesGraph = let
  func m (fin, fout) r f0 fk bw = GomesGraph.Cfg m fin fout r f0 fk bw
  in func
     <$> parseMisoAngle
     <*> parseInOut
     <*> parseRefinementSteps
     <*> parseInitCluster
     <*> parseStepCluster
     <*> parseBadAngle

parseRefinementSteps :: Parser Word8
parseRefinementSteps = option
   (  long "refinement-steps"
   <> short 'n'
   <> metavar "Int"
   <> value 1
   <> help "Number of refinement steps. Default 1")

parseInitCluster :: Parser Double
parseInitCluster = (min 1.2 . abs) <$> option
   (  long "mcl-init-factor"
   <> short 's'
   <> metavar "Double"
   <> value 1.2
   <> help "Initial MCL factor. Default 1.2")

parseStepCluster :: Parser Double
parseStepCluster = let
  func :: Int -> Double
  func = (+ 1) . (/ 100) . fromIntegral . abs
  in func <$> option
   (  long "mcl-increase-factor"
   <> short 'x'
   <> metavar "Int[%]"
   <> value 5
   <> help "Increase ratio of MCL factor. Default 5%")

parseBadAngle :: Parser Deg
parseBadAngle = ((Deg . abs) <$> option
   (  long "bad-fit"
   <> short 'b'
   <> metavar "[Deg]"
   <> value 15
   <> help "The default error is 15 deg."))

-- ======================================= Gomes =========================================

instance ParserCmdLine Gomes.Cfg where
  runAlgo  = Gomes.run
  validate = testInputFile Gomes.ang_input

parseGomes :: Parser Gomes.Cfg
parseGomes = let
  func m gg mg mm (fin, fout) = Gomes.Cfg m gg mg mm fin fout
  in func
     <$> parseMisoAngle
     <*> parseGGAngle
     <*> parseMGAngle
     <*> parseMMAngle
     <*> parseInOut

parseGGAngle :: Parser Deg
parseGGAngle = ((Deg . abs) <$> option
   (  long "gg-angle"
   <> metavar "[Deg]"
   <> value 4
   <> help "The default error is 4 deg."))

parseMGAngle :: Parser Deg
parseMGAngle = ((Deg . abs) <$> option
   (  long "mg-angle"
   <> metavar "[Deg]"
   <> value 4
   <> help "The default error is 4 deg."))

parseMMAngle :: Parser Deg
parseMMAngle = ((Deg . abs) <$> option
   (  long "mm-angle"
   <> metavar "[Deg]"
   <> value 4
   <> help "The default error is 4 deg."))
