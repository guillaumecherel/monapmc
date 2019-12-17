{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util.Figure where

import Protolude

import Data.String (String)
import Data.Text (unpack)
import qualified Data.Text as Text
import System.Process


---- Plot data types
data PlotLine =
  PlotLine (Maybe Text) (Maybe Int) (Maybe Int)
    [(Maybe Text, [Maybe (Double, Double)])]
  -- PlotLine legend color dash [(dataset comment, data)]
  deriving (Show, Eq)

emptyPlotLine :: PlotLine
emptyPlotLine = PlotLine Nothing Nothing Nothing []

appendPlotLine :: PlotLine -> PlotLine -> PlotLine
appendPlotLine
  (PlotLine legend color dash datasets)
  (PlotLine legend' color' dash' datasets')
  = PlotLine
    (legend <|> legend')
    (color <|> color')
    (dash <|> dash')
    (datasets <> datasets')

data PlotBar =
  PlotBar (Maybe Text) (Maybe Int) (Maybe Text)
    [(Maybe Text, [Maybe (Double, Double)])]
  -- PlotBar legend color fillstyle [(dataset comment, data)]
  deriving (Show, Eq)

emptyPlotBar :: PlotBar
emptyPlotBar = PlotBar Nothing Nothing Nothing []

appendPlotBar :: PlotBar -> PlotBar -> PlotBar
appendPlotBar
  (PlotBar legend color style datasets)
  (PlotBar legend' color' style' datasets')
  = PlotBar
    (legend <|> legend')
    (color <|> color')
    (style <|> style')
    (datasets <> datasets')

data PlotPoint =
  PlotPoint (Maybe Text) (Maybe Int) (Maybe Int)
    [(Maybe Text, [Maybe (Double, Double)])]
  -- PlotPoint legend color pointtype [(dataset comment, data)]
  deriving (Show, Eq)

emptyPlotPoint :: PlotPoint
emptyPlotPoint = PlotPoint Nothing Nothing Nothing []

appendPlotPoint :: PlotPoint -> PlotPoint -> PlotPoint
appendPlotPoint
  (PlotPoint legend color style datasets)
  (PlotPoint legend' color' style' datasets')
  = PlotPoint
    (legend <|> legend')
    (color <|> color')
    (style <|> style')
    (datasets <> datasets')

data PlotPointErrDelta =
  PlotPointErrDelta (Maybe Text) (Maybe Int) (Maybe Int)
    [(Maybe Text, [Maybe (Double, Double, Double, Double)])]
  -- PlotPointErrDelta legend color pointtype [(dataset comment, data)]
  deriving (Show, Eq)

emptyPlotPointErrDelta :: PlotPointErrDelta
emptyPlotPointErrDelta = PlotPointErrDelta Nothing Nothing Nothing []

appendPlotPointErrDelta
  :: PlotPointErrDelta
  -> PlotPointErrDelta
  -> PlotPointErrDelta
appendPlotPointErrDelta
  (PlotPointErrDelta legend color style datasets)
  (PlotPointErrDelta legend' color' style' datasets')
  = PlotPointErrDelta
    (legend <|> legend')
    (color <|> color')
    (style <|> style')
    (datasets <> datasets')

data PlotCmd
  = PlotCmdLine PlotLine
  | PlotCmdBar PlotBar
  | PlotCmdPoint PlotPoint
  | PlotCmdPointErrDelta PlotPointErrDelta
  deriving (Show, Eq)

data Plot = Plot [PlotCmd]
  -- Plot [datasets]
  deriving (Show, Eq)

instance Semigroup Plot where
  Plot d1 <> Plot d2 = Plot (d1 <> d2)

data PlotLayout
  = SinglePlot (Maybe Text) Plot
    -- SinglePlot title plot_command
  | Multiplot Orientation (Maybe Text) [[(Text, Plot)]]
    -- Multiplot orientation title [[(plot_title, plot_command)]]
  deriving (Show, Eq)

data Orientation = Row | Column deriving (Show, Eq)

data Figure = Figure FilePath [Text] PlotLayout
  -- Figure prelude plot_layout




---- Build scripts ----

scriptFigure :: Figure -> Text
scriptFigure (Figure figPath prelude layout) =
     Text.unlines prelude <> "\n\n"
  <> "set output \"" <> Text.pack figPath <> "\"\n"
  <> scriptPlotLayout layout

scriptPlot :: Plot -> Text
scriptPlot (Plot xs) =
         "plot \\\n"
      <> (mconcat $ intersperse ", \\\n" $
           flip fmap (zip xs [1..]) (\(datasets, i) -> formatDs datasets i))
         <> "\n"
      -- Inline data
         -- End data for different plot commands by a line containing only
         -- "e"
      <> mconcat (fmap dataInline xs)
  where
    formatDs (PlotCmdLine (PlotLine legend color dashType _)) i =
                "     '-'"
             <> " linecolor " <> show (fromMaybe i color)
             <> " dashtype " <> show (fromMaybe i dashType)
             <> " with line"
             <> maybe mempty (\l -> " title \"" <> l <> "\"") legend
    formatDs (PlotCmdBar (PlotBar legend color fillstyle _)) i =
                "     '-'"
             <> " linecolor " <> show (fromMaybe i color)
             <> " with boxes fillstyle " <> fromMaybe "solid 1.0" fillstyle
             <> maybe mempty (\l -> " title \"" <> l <> "\"") legend
    formatDs (PlotCmdPoint (PlotPoint legend color pointType _)) i =
                "     '-'"
             <> " linecolor " <> show (fromMaybe i color)
             <> " pointtype " <> show (fromMaybe i pointType)
             <> " with points"
             <> maybe mempty (\l -> " title \"" <> l <> "\"") legend
    formatDs (PlotCmdPointErrDelta (PlotPointErrDelta legend color pointType _)) i =
                "     '-'"
             <> " linecolor " <> show (fromMaybe i color)
             <> " pointtype " <> show (fromMaybe i pointType)
             <> " with xyerrorbars"
             <> maybe mempty (\l -> " title \"" <> l <> "\"") legend

scriptPlotLayout :: PlotLayout -> Text
scriptPlotLayout (SinglePlot mbTitle x) =
     maybe mempty (\title -> "set title \"" <> title <> "\"\n") mbTitle
  <> scriptPlot x
scriptPlotLayout (Multiplot orientation mbTitle xss) =
  let (nRows, nColumns) = case orientation of
        Row -> (length xss, maximum $ fmap length xss)
        Column -> (maximum $ fmap length xss, length xss)
      orientationKW = case orientation of
        Row -> "rowsfirst"
        Column -> "columnsfirst"
  in  maybe mempty (\title -> "set title \"" <> title <> "\"\n") mbTitle
   <> "set multiplot layout " <> show nRows <> "," <> show nColumns
     <> " " <> orientationKW <> "\n"
   <> (Text.intercalate "\n"
      $ flip fmap xss (\ xs
       -> Text.intercalate "\n"
        $ take (case orientation of Row -> nColumns; Column -> nRows)
        $ flip fmap xs (\ (plotTitle, plotCmd)
          -> "set title \"" <> plotTitle <> "\"\n"
          <> scriptPlot plotCmd)
       <> repeat "set multiplot next\n"))
   <> "unset multiplot"

dataInline :: PlotCmd -> Text
dataInline (PlotCmdLine (PlotLine _ _ _ dss)) =
   -- Separate different data sets by two blank lines
   Text.intercalate "\n\n"
   ( flip fmap dss (\ (comment,ds)
    -> "#" <> fromMaybe mempty comment <> "\n"
    <> (Text.unlines $ fmap row2Inline ds )))
  <> "e\n"
dataInline (PlotCmdBar (PlotBar _ _ _ dss)) =
     Text.intercalate "\n\n"
     ( flip fmap dss (\ (comment,ds)
     -> "#" <> fromMaybe mempty comment <> "\n"
     <> (Text.unlines $ fmap row2Inline ds )))
  <> "e\n"
dataInline (PlotCmdPoint (PlotPoint _ _ _ dss)) =
     Text.intercalate "\n\n"
     ( flip fmap dss (\ (comment,ds)
     -> "#" <> fromMaybe mempty comment <> "\n"
     <> (Text.unlines $ fmap row2Inline ds )))
  <> "e\n"
dataInline (PlotCmdPointErrDelta (PlotPointErrDelta _ _ _ dss)) =
     Text.intercalate "\n\n"
     ( flip fmap dss (\ (comment,ds)
     -> "#" <> fromMaybe mempty comment <> "\n"
     <> (Text.unlines $ fmap row4Inline ds )))
  <> "e\n"


row2Inline :: Maybe (Double, Double) -> Text
row2Inline Nothing = mempty
row2Inline (Just (x, y)) = show x <> " " <> show y

row4Inline :: Maybe (Double, Double, Double, Double) -> Text
row4Inline Nothing = mempty
row4Inline (Just (x, y, xdelta, ydelta)) =
  show x <> " " <> show y <> " " <> show xdelta <> " " <> show ydelta

formatData :: (a -> Text) -> [(Maybe Text, [a])] -> Text
formatData formatRecord dataSets =
     Text.intercalate "\n\n"
     ( flip fmap dataSets (\ (comment,ds)
     -> "#" <> fromMaybe mempty comment <> "\n"
     <> (Text.unlines $ fmap formatRecord ds )))

formatData2 :: [(Maybe Text, [Maybe (Double, Double)])] -> Text
formatData2 = formatData row2Inline

formatData4 :: [(Maybe Text, [Maybe (Double, Double, Double, Double)])] -> Text
formatData4 = formatData row4Inline

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
gnuplot :: FilePath -> FilePath -> [(String,FilePath)] -> IO ()
gnuplot script output args =
  let gpArgs = [ ("-e" :: String), "outputPath=\"" <> output <> "\"" ]
              <> join ( fmap (\(arg,val) -> ["-e", arg <> "=\"" <> val <> "\""]) 
                             args )
              <> ["-c", script]
  in do
    (status, out, err) <- readProcessWithExitCode ("gnuplot" :: String) gpArgs []
    hPutStrLn stderr err
    hPutStrLn stdout out
    case status of
      ExitSuccess -> return ()
      ExitFailure code -> panic (
        "Error: command gnuplot exited with status " <> show code
        <> "\nFailing command: " <> "gnuplot " <> show gpArgs)


