-- Facade before the utility wrapper ChartRenderer
-- It has chart title and width and height pre-defined
--  so the user needs only to provide the actual points
-- This facade draws onto the PNG file.
module ReverseComplexGraph.Canvas.Png where

import qualified ReverseComplexGraph.Canvas.ChartRenderer as Renderer

defaultTitle = "Lines"
defaultWidth = 800
defaultHeight = 600
defaultFilename = "chart.png" -- TODO set to "images" under `pwd` at least!

-- Render lines on the GTK window
plotLines lines = do
  Renderer.plotLinesToPng lines defaultTitle defaultWidth defaultHeight defaultFilename
  return ()

