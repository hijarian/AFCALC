module AFCalc.AsChart where

-- Модули, необходимые для поддержки черчения графиков функций
import Graphics.Rendering.Chart 
import Graphics.Rendering.Chart.Gtk
import Data.Colour
import Data.Colour.Names
import Data.Accessor

type PointList = [(Double, Double)]
type ADPointList = PointList
type DCPointList = PointList
type CBPointList = PointList
type BAPointList = PointList
type ZPlanePoints = (ADPointList, DCPointList, CBPointList, BAPointList)

----------------------------------------
-- 6. Передаём список точек функции черчения графика, которая чертит график. BEGIN


plotGraph :: String -> PointList -> IO()
plotGraph linetitle datalist = do
    renderableToWindow  (toRenderable (chart linetitle datalist)) 640 480
    renderableToPNGFile (toRenderable (chart linetitle datalist)) 640 480 ("img/" ++ linetitle ++ ".png")
    return ()

plotFullGraph :: String -> ZPlanePoints -> IO()
plotFullGraph linetitle datalists = do
  showFullGraph linetitle datalists
  pngFullGraph linetitle datalists
  return ()

showFullGraph linetitle datalists =
    renderableToWindow  (toRenderable (manychart linetitle datalists)) 640 480
  
pngFullGraph linetitle datalists = 
    renderableToPNGFile (toRenderable (manychart linetitle datalists)) 640 480 ("img/" ++ linetitle ++ ".png")
  

chart :: String -> PointList -> Layout1 Double Double 
chart linetitle datalist = layout
  where
    myPlot = plot_lines_values ^= [datalist]
              $ plot_lines_style .> line_color ^= opaque blue
              $ plot_lines_title ^= linetitle
              $ defaultPlotLines
    layout = layout1_title ^= "Form of blast edge"
           $ layout1_plots ^= [Left (toPlot myPlot)]
           $ defaultLayout1

manychart :: String -> ZPlanePoints -> Layout1 Double Double 
manychart linetitle (pointsAD, pointsDC, pointsCB, pointsBA) = layout
  where
    plotAD = plot_lines_values ^= [pointsAD]
              $ plot_lines_style .> line_color ^= opaque red
              $ plot_lines_title ^= "A->D" 
              $ defaultPlotLines
    plotDC = plot_lines_values ^= [pointsDC]
              $ plot_lines_style .> line_color ^= opaque green 
              $ plot_lines_title ^= "D->C"
              $ defaultPlotLines
    plotCB = plot_lines_values ^= [pointsCB]
              $ plot_lines_style .> line_color ^= opaque blue
              $ plot_lines_title ^= "C->B: " ++ linetitle
              $ defaultPlotLines
    plotBA = plot_lines_values ^= [pointsBA]
              $ plot_lines_style .> line_color ^= opaque silver 
              $ plot_lines_title ^= "B->A" 
              $ defaultPlotLines
    layout = layout1_title ^= "Form of blast edge"
           $ layout1_plots ^= [
             Left (toPlot plotAD), 
             Left (toPlot plotDC), 
             Left (toPlot plotCB), 
             Left (toPlot plotBA)
             ]
           $ defaultLayout1

-- Пример оформления чертежа
-- -- chart = layout
-- --   where
-- --     am :: Double -> Double
-- --     am x = (sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))
-- --     sinusoid1 = plot_lines_values ^= [[ (x,(am x)) | x <- [0,(0.5)..400]]]
-- --               $ plot_lines_style  .> line_color ^= opaque blue
-- --               $ plot_lines_title ^= "am"
-- --               $ defaultPlotLines
-- --     sinusoid2 = plot_points_style ^= filledCircles 2 (opaque red)
-- --               $ plot_points_values ^= [ (x,(am x)) | x <- [0,7..400]]
-- --               $ plot_points_title ^= "am points"
-- --               $ defaultPlotPoints
-- --     layout = layout1_title ^= "Amplitude Modulation"
-- --            $ layout1_plots ^= [Left (toPlot sinusoid1),
-- --                                Left (toPlot sinusoid2)]
-- --            $ defaultLayout1

-- 6. Передаём список точек функции черчения графика, которая чертит график. END
----------------------------------------
