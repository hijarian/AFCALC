module AFCalc.AsChart where
-- Module to actually plot points generated by main AFCalc module as charts

-- We will use Chart package for rendering
-- WARNING: Graphics.Rendering.Chart.Gtk is a pain in the ass to build in Windows systems
--   as of 2011.09.01
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk

-- We need this helper modules to configure Charts comfortably
import Data.Colour
import Data.Colour.Names
import Data.Accessor

-- We will use PointList types defined in AFCalc
import AFCalc

-- Default width
defw = 1024
-- Default height
defh = 768

--------------------------------------------------------------------------------
-- "Charting" of lines

-- Output PointList to GTK window
plotLine :: String -> PointList -> IO()
plotLine linetitle datalist = do
    renderableToWindow  (toRenderable (chart linetitle datalist)) defw defh
    return ()

-- Output PointList to file with name <filename>
pngLine :: String -> String -> PointList -> IO()
pngLine filename linetitle datalist = do
    renderableToPNGFile (toRenderable (chart linetitle datalist)) defw defh filename
    return ()

-- Output ZPlanePoints to GTK window
plotArea :: String -> ZPlanePoints -> IO()
plotArea linetitle datalist = do
    renderableToWindow  (toRenderable (manychart linetitle datalist)) defw defh
    return ()

-- Output ZPlanePoints to file with name <filename>
pngArea :: String -> String -> ZPlanePoints -> IO()
pngArea filename linetitle datalist = do
    renderableToPNGFile (toRenderable (manychart linetitle datalist)) defw defh filename
    return ()

-- Configuration of chart with single line
chart :: String -> PointList -> Layout1 Double Double
chart linetitle datalist = layout'
  where
    plot' = myPlot (opaque blue) linetitle datalist
    layout' = myLayout ("Line " ++ linetitle) [plot']

-- Configuration of chart with four lines representing sides of area
manychart :: String -> ZPlanePoints -> Layout1 Double Double
manychart linetitle (pointsAB, pointsBC, pointsCD, pointsDA) = layout'
  where
    plotAB' = myPlot (opaque red)   "A->B" pointsAB
    plotBC' = myPlot (opaque green) "B->C" pointsBC
    plotCD' = myPlot (opaque blue)  "C->D" pointsCD
    plotDA' = myPlot (opaque black) "D->A" pointsDA
    layout' = myLayout "Area of Z variable" [plotAB', plotBC', plotCD', plotDA']

-- Generating plot object with given parameters
myPlot :: AlphaColour Double -> String -> PointList -> PlotLines Double Double
myPlot color title datalist = plot_lines_values ^= [datalist]
                            $ plot_lines_style .> line_color ^= color
                            $ plot_lines_title ^= title
                            $ defaultPlotLines

-- Generating layout object with given parameters
-- <plotlist> is a list of object created by myPlot
myLayout :: String -> [PlotLines Double Double] -> Layout1 Double Double
myLayout title plotlist = layout1_title ^= title
                        $ layout1_plots ^= plotlist'
                        $ defaultLayout1
                            where  plotlist' = map (\p -> Left (toPlot p)) plotlist
