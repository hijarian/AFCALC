-- Entry point of the whole ReverseComplexGraph program
-- 
module Main where

import qualified ReverseComplexGraph.Canvas.Png as PngRenderer
import qualified ReverseComplexGraph.Canvas.Gtk as GtkRenderer

main = do
  GtkRenderer.plotLines lines
  PngRenderer.plotLines lines 
  return ()
    where
      lines = [
        [ (6, 6), (6, 1), (6, 2), (6, 3)],
        [ (1, 6), (2, 6), (3, 6), (4, 6)]
        ]

