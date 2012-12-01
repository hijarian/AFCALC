module ReverseComplexGraph.Canvas.ChartRenderer (
  plotLinesToPng,
  plotLinesToWindow
  ) where

-- We will use Chart package for rendering
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
-- We need this helper modules to configure Charts comfortably
import Data.Colour
import Data.Colour.Names
import Data.Accessor

type ChartLines = [ChartLine]
type ChartLine = [ChartPoint]
type ChartPoint = (XCoord, YCoord)
type XCoord = Double
type YCoord = Double

type ChartTitle = String
type ChartLineTitle = String
type Filename = String

type ChartWidth = Int
type ChartHeight = Int

type StyledPlotLine = PlotLines Double Double
type StyledChart = Layout1 Double Double

--------------------------------------------------------------------------------
-- Public Interface

plotLinesToPng :: ChartLines -> ChartTitle -> ChartWidth -> ChartHeight -> Filename -> IO()
plotLinesToPng lines title width height filename = do
  renderableToPNGFile (myRenderable title lines) width height filename
  return ()

plotLinesToWindow :: ChartLines -> ChartTitle -> ChartWidth -> ChartHeight -> IO()
plotLinesToWindow lines title width height = do
  renderableToWindow (myRenderable title lines) width height
  return ()
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Auxiliary functions

myRenderable :: ChartTitle -> ChartLines -> Renderable() 
myRenderable title lines = toRenderable (myLayout title plots)
  where
    plots = map makePlotLine lines

-- Generating layout object with given parameters
-- <plotlist> is a list of object created by myPlot
myLayout :: ChartTitle -> [StyledPlotLine] -> StyledChart
myLayout title plotlines = layout1_title ^= title
                       $ layout1_plots ^= map toLeftPlot plotlines
                       $ defaultLayout1
  where  
    toLeftPlot plotline = Left (toPlot plotline)

-- Make PlotLines object required by the Chart library from our simple point list
-- TODO: this method should automatically create the title and color for the line.
-- Color should be random, and titles should be the succession of "A->B", "B->C", "C->D", "D->A".
makePlotLine :: ChartLine -> StyledPlotLine
makePlotLine line = myPlotLine makeRandomColor makeNextLineTitle line
  where
    makeRandomColor = (opaque red) -- TODO
    makeNextLineTitle = "LINE"
  
-- Generate the colored plot line with title
-- Note that the PlotLines object holds *list* of point lists, so, we can have several
-- lines with the same color and title. But here we won't do this.
myPlotLine :: AlphaColour Double -> ChartLineTitle -> ChartLine -> StyledPlotLine
myPlotLine color title pointline = plot_lines_values ^= [pointline] -- Note that we wrap single points line into a list, because we do not want several lines to be in a single PlotLines object.
                                  $ plot_lines_style .> line_color ^= color
                                  $ plot_lines_title ^= title
                                  $ defaultPlotLines
