{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import qualified Arche.OR                   as OR
import qualified Arche.Strategy.Graph       as Graph
import qualified Arche.Strategy.ORFitAll    as ORFitAll
import qualified Arche.Strategy.GomesGraph  as GomesGraph

import           System.Directory            (doesFileExist)
import           Control.Monad               (when)
import           Data.Word                   (Word8)

import           Options.Applicative
import           System.FilePath

import           Texture.Orientation         (Deg(..), mkAxisPair, AxisPair)
import           Linear.Vect

-- ===================================== Data & class ====================================

class ParserCmdLine a where
  runAlgo  :: a -> FilePath -> FilePath -> IO ()
  validate :: FilePath -> a -> IO (Either a String)

data RunMode = forall a . ParserCmdLine a => RunMode
  { config :: a
  , io_files :: (FilePath, FilePath)
  }

-- ========================================= Main ========================================

parseMode :: Parser RunMode
parseMode = subparser
 ( command "micro-features"
   (info (RunMode <$> parseShowGraph <*> parseInOut)
   (progDesc "Identify grain's ID, vertexes, edges and faces."))
 <> command "optimum-OR"
   (info (RunMode <$> parseORFitAll <*> parseInOut)
   (progDesc "Finds the best Orientation Relationship."))
 <> command "reconstruction"
   (info (RunMode <$> parseGomesGraph <*> parseInOut)
   (progDesc "Reconstruction based on Gomes's method (graph clustering)."))
 )

main :: IO ()
main = do
  RunMode mode (file_in, file_out) <- execParser opts
  cfg <- validate file_in mode
  case cfg of
    Left  c -> runAlgo c file_in file_out
    Right s -> error s
  where
    opts = info (helper <*> parseMode)
           ( fullDesc
           <> progDesc "Reconstructs arche phase from EBSD data"
           <> header "Arche Builder" )

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
parseMisoAngle = ((Deg . abs) <$> option auto
   (  long "grain-miso"
   <> short 'm'
   <> metavar "[Deg]"
   <> value 15
   <> help "The default error is 15 deg."))

parseOR :: Parser AxisPair
parseOR = let
  func :: (Int, Int, Int, Double) -> AxisPair
  func (v1, v2, v3, w) = mkAxisPair v (Deg w)
      where v = Vec3 (fromIntegral v1) (fromIntegral v2) (fromIntegral v3)
  in (func <$> option auto
    (  long "or"
    <> short 'r'
    <> metavar "\"(Int,Int,Int,Double)\""
    ))

parseStartOR :: Parser AxisPair
parseStartOR = let
  func :: (Int, Int, Int, Double) -> AxisPair
  func (v1, v2, v3, w) = mkAxisPair v (Deg w)
      where v = Vec3 (fromIntegral v1) (fromIntegral v2) (fromIntegral v3)
  in (func <$> option auto
    (  long "start-or"
    <> metavar "\"(Int,Int,Int,Double)\""
    ))

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

testInputFile :: FilePath -> a -> IO (Either a String)
testInputFile file cfg = do
    inOk <- goodInputFile file
    return $ if inOk
      then Left cfg
      else Right "Failed to read the input file."

-- ========================================= ConnGraph ===================================

instance ParserCmdLine Graph.Cfg where
  runAlgo  = Graph.run
  validate = testInputFile

parseShowGraph :: Parser Graph.Cfg
parseShowGraph = Graph.Cfg
  <$> parseMisoAngle
  <*> parseParent
  <*> parseProduct

-- ======================================= OR Fit All ====================================

instance ParserCmdLine ORFitAll.Cfg where
  runAlgo  = ORFitAll.run
  validate = testInputFile

parseORFitAll :: Parser ORFitAll.Cfg
parseORFitAll = ORFitAll.Cfg
  <$> parseMisoAngle
  <*> parseORbyAvg
  <*> optional parseOR
  <*> optional parseStartOR
  <*> parseParent
  <*> parseProduct

parseORbyAvg :: Parser Bool
parseORbyAvg = switch
   (  long "or-by-avg"
   <> short 'a'
   <> help "Uses average grain orientation when optimizing OR")

-- ================================== GomesGraph =========================================

instance ParserCmdLine GomesGraph.Cfg where
  runAlgo  = GomesGraph.run
  validate = testInputFile

parseGomesGraph :: Parser GomesGraph.Cfg
parseGomesGraph = GomesGraph.Cfg
  <$> parseMisoAngle
  <*> parseExtMCL
  <*> parseNoFloatingGrains
  <*> parseRefinementSteps
  <*> parseInitCluster
  <*> parseStepCluster
  <*> parseBadAngle
  <*> (OR.convert <$> parseOR)
  <*> parseProduct
  <*> parseParent
  <*> parseOutputToANG
  <*> parseOutputToCTF

parseRefinementSteps :: Parser Word8
parseRefinementSteps = option auto
   (  long "refinement-steps"
   <> short 'n'
   <> metavar "Int"
   <> value 1
   <> help "Number of refinement steps. Default 1")

parseInitCluster :: Parser Double
parseInitCluster = (max 1.2 . abs) <$> option auto
   (  long "mcl-init-factor"
   <> short 's'
   <> metavar "Double"
   <> value 1.2
   <> help "Initial MCL factor. Default 1.2")

parseStepCluster :: Parser Double
parseStepCluster = let
  func :: Int -> Double
  func = (+ 1) . (/ 100) . fromIntegral . abs
  in func <$> option auto
   (  long "mcl-increase-factor"
   <> short 'x'
   <> metavar "Int[%]"
   <> value 5
   <> help "Increase ratio of MCL factor. Default 5%")

parseBadAngle :: Parser Deg
parseBadAngle = ((Deg . abs) <$> option auto
   (  long "bad-fit"
   <> short 'b'
   <> metavar "[Deg]"
   <> value 5
   <> help "The default error is 5 deg."))

parseParent :: Parser (Either OR.Phase OR.PhaseSymm)
parseParent = let
  left  = fmap Left  $ OR.Phase <$> parseParentPhaseID <*> parseParentSymm
  right = fmap Right $ parseParentSymm
  in left <|> right

parseProduct :: Parser OR.Phase
parseProduct = OR.Phase <$> parseProductPhaseID <*> parseProductSymm

parseParentPhaseID :: Parser Int
parseParentPhaseID = option auto
   (  long "parentPhase"
   <> metavar "Int"
   <> help "ID number of parent phase in the ANG/CTF file, if present.")

parseProductPhaseID :: Parser Int
parseProductPhaseID = option auto
   (  long "productPhase"
   <> metavar "Int"
   <> help "ID number of product phase in the ANG/CTF file, if present.")

symmReader :: String -> Maybe OR.PhaseSymm
symmReader x = case x of
  "cubic"     -> Just OR.CubicPhase
  "hexagonal" -> Just OR.HexagonalPhase
  "bcc"       -> Just OR.CubicPhase
  "fcc"       -> Just OR.CubicPhase
  "hcp"       -> Just OR.HexagonalPhase
  _           -> Nothing

parseParentSymm :: Parser OR.PhaseSymm
parseParentSymm = option (maybeReader symmReader)
  (  long "parentSymmetry"
   <> metavar "symmetry"
   <> help "Type of symmetry on parent phase.")

parseProductSymm :: Parser OR.PhaseSymm
parseProductSymm = option (maybeReader symmReader)
  (  long "productSymmetry"
   <> metavar "symmetry"
   <> help "Type of symmetry on product phase.")

parseExtMCL :: Parser Bool
parseExtMCL = switch
   (  long "extMCL"
   <> help "Uses external software for clustering. Requires MCL installed")

parseNoFloatingGrains :: Parser Bool
parseNoFloatingGrains = switch
   (  long "excludeFloatingGrains"
   <> help "Exclude floating grains (grains without junctions) from the reconstruction.")

parseOutputToANG :: Parser Bool
parseOutputToANG = switch
   (  long "ang"
   <> help "Generate output OIM in ANG format.")

parseOutputToCTF :: Parser Bool
parseOutputToCTF = switch
   (  long "ctf"
   <> help "Generate output OIM in CTF format.")
