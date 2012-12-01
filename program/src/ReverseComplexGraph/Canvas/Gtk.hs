-- Facade before the utility wrapper ChartRenderer
-- It has chart title and width and height pre-defined
--  so the user needs only to provide the actual points
-- This facade draws on the GTK window.
module ReverseComplexGraph.Canvas.Gtk where

import qualified ReverseComplexGraph.Canvas.ChartRenderer as Renderer

defaultTitle = "Lines"
defaultWidth = 800
defaultHeight = 600

-- Render lines on the GTK window
plotLines lines = do
  Renderer.plotLinesToWindow lines defaultTitle defaultWidth defaultHeight
  return ()
