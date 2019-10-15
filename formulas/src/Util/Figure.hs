{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util.Figure where

import Protolude

-- import Control.Monad.Random.Lazy
-- import Data.Functor.Compose
import qualified Data.Set as Set
import Data.String (String)
import Data.Text (unpack)
import qualified Data.Text as Text
import System.Process

import Data.Cached

data Datasets
  = DataLine Text (Maybe Int) (Maybe Int) [(Text, [Maybe (Double, Double)])]
    -- DataLine legend color pointtype [(dataset comment, data)]
  | DataBar Text (Maybe Int) (Maybe Text) [(Text, [(Double, Double)])]
    -- DataBar legend color fillstyle [(dataset comment, data)]
  | DataPoint Text (Maybe Int) (Maybe Int) [(Text, [(Double, Double)])]
    -- DataPoint legend color pointtype [(dataset comment, data)]
  | DataPointErrDelta Text (Maybe Int) (Maybe Int)
    [(Text, [(Double, Double, Double, Double)])]
    -- DataPointErrDelta legend color pointtype [(dataset comment, data)]
  deriving (Show, Eq)

data Plot = Plot Text [Datasets]
  deriving (Show, Eq)
  -- Plot title [(legend, color, datasets)]

instance Semigroup Plot where
  Plot t1 d1 <> Plot t2 d2 = Plot (t1 <> t2) (d1 <> d2)

data PlotLayout
  = SinglePlot Plot
    -- SinglePlot plot_command
  | Multiplot Orientation Text [[Plot]]
    -- Multiplot orientation title plot_commands
  deriving (Show, Eq)

data Orientation = Row | Column deriving (Show, Eq)

data Figure = Figure FilePath [Text] PlotLayout
  -- Figure path prelude plot_layout


---- Build scripts ----

scriptFigure :: Figure -> Text
scriptFigure (Figure figPath prelude layout) =
     Text.unlines prelude <> "\n\n"
  <> "set output '" <> Text.pack figPath <> "'\n"
  <> scriptPlotLayout layout

scriptPlot :: Plot -> Text
scriptPlot (Plot title xs) =
         "set title '" <> title <> "'\n"
      <> "plot \\\n"
      <> (mconcat $ intersperse ", \\\n" $
           flip fmap (zip xs [1..]) (\(datasets, i) -> formatDs datasets i))
         <> "\n"
      -- Inline data
         -- End data for different plot commands by a line containing only
         -- "e"
      <> mconcat (fmap dataInline xs)
  where
    formatDs (DataLine legend color pointType _) i =
                "     '-'"
             <> " linecolor " <> show (fromMaybe i color)
             <> " pointtype " <> show (fromMaybe i pointType)
             <> " with line"
             <> " title '" <> legend <> "'"
    formatDs (DataBar legend color fillstyle _) i =
                "     '-'"
             <> " linecolor " <> show (fromMaybe i color)
             <> " with boxes fillstyle " <> fromMaybe "solid 1.0" fillstyle
             <> " title '" <> legend <> "'"
    formatDs (DataPoint legend color pointType _) i =
                "     '-'"
             <> " linecolor " <> show (fromMaybe i color)
             <> " pointtype " <> show (fromMaybe i pointType)
             <> " with points"
             <> " title '" <> legend <> "'"
    formatDs (DataPointErrDelta legend color pointType _) i =
                "     '-'"
             <> " linecolor " <> show (fromMaybe i color)
             <> " pointtype " <> show (fromMaybe i pointType)
             <> " with xyerrorbars"
             <> " title '" <> legend <> "'"

scriptPlotLayout :: PlotLayout -> Text
scriptPlotLayout (SinglePlot x) = scriptPlot x
scriptPlotLayout (Multiplot orientation title xss) =
  let (nRows, nColumns) = case orientation of
        Row -> (length xss, maximum $ fmap length xss)
        Column -> (maximum $ fmap length xss, length xss)
      orientationKW = case orientation of
        Row -> "rowsfirst"
        Column -> "columnsfirst"
  in  "set title '" <> title <> "'\n"
   <> "set multiplot layout " <> show nRows <> "," <> show nColumns
     <> " " <> orientationKW <> "\n"
   <> (Text.intercalate "\n"
      $ flip fmap xss (\ xs
       -> Text.intercalate "\n"
        $ take (case orientation of Row -> nColumns; Column -> nRows)
        $ fmap scriptPlot xs <> repeat "set multiplot next\n"))
   <> "unset multiplot"

dataInline :: Datasets -> Text
dataInline (DataLine _ _ _ dss) =
   -- Separate different data sets by two blank lines
   Text.intercalate "\n\n"
   ( flip fmap dss (\ (comment,ds)
    -> "#" <> comment <> "\n"
    <> (Text.unlines $ flip fmap ds (\row ->
           case row of
             Just (x, y) -> show x <> " " <> show y
             Nothing -> ""))))
  <> "e\n"
dataInline (DataBar _ _ _ dss) =
     Text.intercalate "\n\n"
     ( flip fmap dss (\ (comment,ds)
     -> "#" <> comment <> "\n"
     <> (Text.unlines $ flip fmap ds (\(x,y) ->
           show x <> " " <> show y))))
  <> "e\n"
dataInline (DataPoint _ _ _ dss) =
     Text.intercalate "\n\n"
     ( flip fmap dss (\ (comment,ds)
     -> "#" <> comment <> "\n"
     <> (Text.unlines $ flip fmap ds (\(x,y) ->
           show x <> " " <> show y))))
  <> "e\n"
dataInline (DataPointErrDelta _ _ _ dss) =
     Text.intercalate "\n\n"
     ( flip fmap dss (\ (comment,ds)
     -> "#" <> comment <> "\n"
     <> (Text.unlines $ flip fmap ds
         (\ (x, y, xdelta, ydelta) ->
                  show x <> " " <> show y <> " "
               <> show xdelta <> " " <> show ydelta))))
  <> "e\n"





---- Make plots ----

-- Run gnuplot with given text script. Optionally save script to given path.
gnuplotInline :: Figure -> IO ()
gnuplotInline fig@(Figure path _ _) = do
  writeFile (path <> ".gnuplot.script") script
  out <- readProcess ("gnuplot"::String) [] (unpack script)
  putStrLn out
  return ()
  where script = scriptFigure fig
 
-- Run gnuplot reading script from path, passing given args with gnuplot option "-e" and writing figure to output path.
gnuplot :: FilePath -> FilePath -> [(String,FilePath)] -> Cached ()
gnuplot script output args = trigger output
                                     (command)
                                     (Set.fromList $ script:fmap snd args)
  where command = do
          (status, out, err) <- readProcessWithExitCode ("gnuplot" :: String) gpArgs []
          hPutStrLn stderr err
          hPutStrLn stdout out
          case status of
            ExitSuccess -> return ()
            ExitFailure code -> panic (
              "Error: command gnuplot exited with status " <> show code
              <> "\nFailing command: " <> "gnuplot " <> show gpArgs)
        gpArgs = [ ("-e" :: String), "outputPath='" <> output <> "'" ]
              <> join ( fmap (\(arg,val) -> ["-e", arg <> "='" <> val <> "'"]) 
                             args )
              <> ["-c", script]


