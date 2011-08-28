module BlastModel.AsChart where

import BlastModel.Simple

-- Модули, необходимые для поддержки черчения графиков функций
import Graphics.Rendering.Chart 
import Graphics.Rendering.Chart.Gtk
import Data.Colour
import Data.Colour.Names
import Data.Accessor

type PointList = [(Double, Double)]
type BAPointList = PointList
type CDPointList = PointList
type CBPointList = PointList
type DAPointList = PointList
type ZPlanePoints = (CBPointList, CDPointList, DAPointList, BAPointList)

----------------------------------------
-- 6. Передаём список точек функции черчения графика, которая чертит график. BEGIN

extract_param_names param = 
  let phi0   = show $ phi_0 param
      v0     = show $ v_0 param
      alpha0 = show $ alpha param
      a0     = show $ a param
      b0     = show $ b param
      abstau = show $ tau param
      precision0 = show $ precision param
  in "Phi0 = " ++ phi0 ++ ", V0 = " ++ v0 ++ ", Alpha0 = " ++ alpha0 ++ ", A = " ++ a0 ++ ", B = " ++ b0 ++ ", tau = " ++ abstau ++ ", precision = " ++ precision0

plotGraph :: String -> PointList -> IO()
plotGraph linetitle datalist = do
    renderableToWindow  (toRenderable (chart linetitle datalist)) 640 480
    renderableToPNGFile (toRenderable (chart linetitle datalist)) 640 480 (linetitle ++ ".png")
    return ()

plotFullGraph :: String -> ZPlanePoints -> IO()
plotFullGraph linetitle datalists = do
  showFullGraph linetitle datalists
  pngFullGraph linetitle datalists
  return ()

showFullGraph linetitle datalists =
    renderableToWindow  (toRenderable (manychart linetitle datalists)) 640 480
  
pngFullGraph linetitle datalists = 
    renderableToPNGFile (toRenderable (manychart linetitle datalists)) 640 480 (linetitle ++ ".png")
  

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
manychart linetitle (pointsCB, pointsCD, pointsBA, pointsDA) = layout
  where
    plotBA = plot_lines_values ^= [pointsBA]
              $ plot_lines_style .> line_color ^= opaque green 
              $ plot_lines_title ^= "BA" 
              $ defaultPlotLines
    plotDA = plot_lines_values ^= [pointsDA]
              $ plot_lines_style .> line_color ^= opaque red
              $ plot_lines_title ^= "DA" 
              $ defaultPlotLines
    plotCB = plot_lines_values ^= [pointsCB]
              $ plot_lines_style .> line_color ^= opaque blue
              $ plot_lines_title ^= "CB: " ++ linetitle
              $ defaultPlotLines
    plotCD = plot_lines_values ^= [pointsCD]
              $ plot_lines_style .> line_color ^= opaque cyan 
              $ plot_lines_title ^= "CD"
              $ defaultPlotLines
    layout = layout1_title ^= "Form of blast edge"
           $ layout1_plots ^= [
             Left (toPlot plotBA), 
             Left (toPlot plotDA), 
             Left (toPlot plotCD), 
             Left (toPlot plotCB)
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
