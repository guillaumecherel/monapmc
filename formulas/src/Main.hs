{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Applicative
import qualified Control.Foldl as L
import Control.Monad
import Data.Foldable (foldl')
import Data.Functor.Classes
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
import qualified Statistics.Sample.Histogram as S
import System.Environment
import System.Exit
import Text.Read
import qualified Text.Parsec as P

normal :: Double -> Double -> Double -> Double
normal x mean var =
  exp(- ((x - mean) ** 2.0) / (2.0 * var)) / sqrt(2.0 * pi * var)

histogram :: Double ->  Double -> Int -> [Double] -> [(Double, Double)]
histogram lowerBound upperBound bins xs =
  let every = (upperBound - lowerBound) / fromIntegral bins
      lowerBoundBins = [lowerBound, lowerBound + every .. upperBound]
      statHist = V.toList $ S.histogram_ bins lowerBound upperBound (V.fromList $ filter (\x -> x > lowerBound && x <= upperBound) xs)
  in zip lowerBoundBins statHist


scaledHistogram :: Double -> Double -> Int -> [Double] -> [(Double, Double)]
scaledHistogram lowerBound upperBound bins xs =
  let hist = histogram lowerBound upperBound bins xs
      binWidth = (upperBound - lowerBound) / fromIntegral bins
      scalingFactor = 1.0 / (sum (map snd hist) * binWidth)
  in map (\(x, h) -> (x, h * scalingFactor)) hist

read1DSample :: FilePath -> T.Text -> Either P.ParseError [Double]
read1DSample = --mapM ((fmap fst) . TR.double) (T.lines text)
  P.parse parser1DSample

parserInt :: (P.Stream s m Char) => P.ParsecT s u m Int
parserInt = read <$> (P.option "" (P.string "-") <> P.many1 P.digit)

parserDouble :: (P.Stream s m Char) => P.ParsecT s u m Double
parserDouble = read <$> (P.option "" (P.string "-") <> P.many1 P.digit <> P.option "" (P.string "." <> P.many P.digit) <> P.option "" (P.string "e" <> P.option "" (P.string "-") <> P.many1 P.digit))

-- TODO: problème quand il y a un "\n" après le dernier double
parser1DSample :: (P.Stream s m Char) => P.ParsecT s u m [Double]
parser1DSample = P.many $ P.try parserDouble <* P.optional P.endOfLine
  --P.sepBy parserDouble P.endOfLine << P.endOfLine

columns2 :: (Show a, Show b) => String -> [(a, b)] -> String
columns2 _ [] = ""
columns2 sep ((a,b):xs) = show a ++ sep ++ show b ++ "\n" ++ columns2 sep xs

data Algorithm =
  Lenormand2012
    { getN :: Int
    , getAlpha :: Double
    , getPAccMin :: Double
    }
  | Beaumont2009
    { getN :: Int
    , getEpsilonFrom :: Double
    , getEpsilonTo :: Double
    }
  | SteadyState
    { getN :: Int
    , getAlpha :: Double
    , getPAccMin :: Double
    , getParallel :: Int
    }
  deriving (Show, Eq, Ord)

data SimulationResult = SimulationResult
  { getAlgorithm :: Algorithm
  , getStep :: Int
  , getReplication:: Int
  , getSample:: [Double]
  }
  deriving (Show, Eq, Ord)

loadSimulation :: FilePath -> IO (Either P.ParseError SimulationResult)
loadSimulation f = do
  simuOutput <- fmap (read1DSample f) (TIO.readFile f)
  return (P.parse parseSimulationFileName ("filename " ++ f) f <*> simuOutput)
  where parseSimulationFileName = P.try parseLenormand2012 <|> P.try parseBeaumont2009 <|> parseSteadyState

parseLenormand2012 :: (P.Stream s m Char) => P.ParsecT s u m ([Double] -> SimulationResult)
parseLenormand2012 = do
  parserSkipDirname
  P.string "lenormand2012"
  n <- P.char '_' *> parserInt
  alpha <- P.char '_' *> parserDouble
  pAccMin <- P.char '_' *> parserDouble
  step <- P.char '_' *> parserInt
  replication <- P.char '_' *> parserInt
  ext <- P.string ".csv"
  return $ SimulationResult (Lenormand2012 n alpha pAccMin) step replication

parseBeaumont2009 :: (P.Stream s m Char) => P.ParsecT s u m ([Double] -> SimulationResult)  
parseBeaumont2009 = do
  parserSkipDirname *> P.string "beaumont2009"
  n <- P.char '_' *> parserInt
  epsilonFrom <- P.char '_' *> parserDouble
  epsilonTo <- P.char '_' *> parserDouble
  step <- P.char '_' *> parserInt
  replication <- P.char '_' *> parserInt
  P.string ".csv"
  return $ SimulationResult (Beaumont2009 n epsilonFrom epsilonTo) step replication

parseSteadyState :: (P.Stream s m Char) => P.ParsecT s u m ([Double] -> SimulationResult)  
parseSteadyState = do
  parserSkipDirname *> P.string "steadyState"
  n <- P.char '_' *> parserInt
  alpha <- P.char '_' *> parserDouble
  pAccMin <- P.char '_' *> parserDouble
  parallel <- P.char '_' *> parserInt
  step <- P.char '_' *> parserInt
  replication <- P.char '_' *> parserInt
  P.string ".csv"
  return $ SimulationResult (SteadyState n alpha pAccMin parallel) step replication

parserSkipDirname :: (P.Stream s m Char) => P.ParsecT s u m ()
parserSkipDirname = P.optional $ P.many (P.try $ P.many (P.noneOf "/") <> P.string "/")

toyPosterior :: Double -> Double
toyPosterior x =
  0.5 * normal x 0.0 (1.0 / 100.0) + 0.5 * normal x 0.0 1.0

toyPosteriorSample :: Double -> Double -> Int -> [(Double, Double)]
toyPosteriorSample lowerBound upperBound samples =
  let every = (upperBound - lowerBound) / fromIntegral samples
  in [(x, toyPosterior x) | x <- [lowerBound, lowerBound + every .. upperBound]]

toyPosteriorScaledHistogram :: Double -> Double -> Int -> [Double] -> [(Double, Double)]
-- toyPosteriorScaledHistogram = scaledHistogram
toyPosteriorScaledHistogram lowerBound upperBound bins xs = 
  let hist = histogram lowerBound upperBound bins xs
      counts = map snd hist
      binWidth = fst (head hist) - fst (head $ tail hist)
      density = map toyPosterior [x + (binWidth / 2.0) | (x, _) <- hist]
      scalingFactor = foldl' (\s (c, d) -> s + c * d) 0 (zip counts density) / foldl' (\s d -> s + d ** 2) 0 counts
  in map (fmap (* scalingFactor)) hist

posteriorL2 :: (Double -> Double) -> [Double] -> Double
posteriorL2 density xs =
  let lowerBound = -10
      upperBound = 10
      bins = 300
      scaledHist = toyPosteriorScaledHistogram lowerBound upperBound bins xs
      binWidth = (upperBound - lowerBound) / fromIntegral bins
      density = [toyPosterior (x + (binWidth / 2.0)) | (x,_) <- scaledHist]
      scaledHistHeights = map snd scaledHist
  in sqrt $ getSum $ foldMap (\(d,h) -> Sum $ (d - h) ** 2) (zip density scaledHistHeights)

toyPosteriorL2 :: [Double] -> Double
toyPosteriorL2 = posteriorL2 toyPosterior

toyPosteriorL2Mean :: L.Fold SimulationResult Double
toyPosteriorL2Mean = L.premap (toyPosteriorL2 . getSample) L.mean

numberSimusMean :: L.Fold SimulationResult Double
numberSimusMean = L.premap (fromIntegral . numberSimus) L.mean

groupReplications :: L.Fold SimulationResult r -> L.Fold SimulationResult (M.Map Algorithm r)
groupReplications = L.groupBy getAlgorithm

numberSimus :: SimulationResult -> Int
numberSimus SimulationResult {getAlgorithm=Lenormand2012 {getN=n, getAlpha=alpha}, getStep=step} = numberSimusLenormand2012 n (floor $ fromIntegral n * alpha) step
numberSimus SimulationResult {getStep=step} = numberSimusSteadyState step

numberSimusLenormand2012 :: Int -> Int -> Int -> Int
numberSimusLenormand2012 n nAlpha step = n + (n - nAlpha) * step

numberSimusSteadyState :: Int -> Int
numberSimusSteadyState step = step

readStepsAndFiles :: [String] -> Either String [(Int, String)]
readStepsAndFiles [] = Right []
readStepsAndFiles [x] = Left $ "Missing algorithm step or filename: " ++ show (x,())
readStepsAndFiles (stepsStr:filename:xs) =
  case readMaybe stepsStr of
    Nothing -> Left $ "Couldn't read steps: " ++ stepsStr
    Just steps -> do
      remaining <- readStepsAndFiles xs
      Right $ (steps, filename):remaining

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["toy_posterior_sample",lowerBound,upperBound, samples] ->
      putStrLn $ columns2 " " $ toyPosteriorSample (read lowerBound) (read upperBound) (read samples)

    ["toy_histogram", simuOutputFile, lowerBound, upperBound, bins] -> do
      parseResult <- loadSimulation simuOutputFile
      case parseResult of
        Left err -> die $ show err
        Right simRes -> putStrLn $ columns2 " " $ histogram (read lowerBound) (read upperBound) (read bins) (getSample simRes)

    ["toy_scaled_histogram", simuOutputFile, lowerBound, upperBound, bins] -> do
      parseResult <- loadSimulation simuOutputFile
      case parseResult of
        Left err -> die ("Error reading file: " ++ simuOutputFile ++ "\n" ++ show err)
        Right simRes -> putStrLn $ columns2 " " $ toyPosteriorScaledHistogram (read lowerBound) (read upperBound) (read bins) (getSample simRes)

    ["toy_posterior_L2", simuOutputFile] -> do
      parseResult <- loadSimulation simuOutputFile
      case parseResult of
        Left err -> die ("Error reading file: " ++ simuOutputFile ++ "\n" ++ show err)
        Right simRes -> print $ toyPosteriorL2 (getSample simRes)

    ["number_simulations_lenormand2012", n, nAlpha, step] -> print $ numberSimusLenormand2012 (read n) (read nAlpha) (read step)

    ["number_simulations_steadyState", step] -> print $ numberSimusSteadyState (read step)

    "L2_vs_number_simulations":files -> do
           parseResults <- traverse loadSimulation files
           case sequence parseResults of
             Left err -> die ("Error reading file: " ++ show err)
             Right simRess -> putStrLn $ columns2 " " $ (M.elems) $ L.fold (groupReplications ((,) <$> numberSimusMean <*> toyPosteriorL2Mean)) simRess
             -- [(numberSimus simRes, toyPosteriorL2 (getSample simRes)) | simRes <- simRess]

    _ -> putStrLn $ "Unknown command " ++ show args
