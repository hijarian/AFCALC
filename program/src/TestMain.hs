-- Entry point of the whole ReverseComplexGraph program
-- 
module Main where

import qualified ReverseComplexGraph.LoggedCalculation as ReverseMapping

import qualified ReverseComplexGraph.Canvas.Png as PngImage
import qualified ReverseComplexGraph.Canvas.Gtk as GtkWindow

import qualified TestModel.Functions as ModelFunctions
import qualified TestModel.Lines as ModelData

main = do
  render $ calculate $ input
  return ()

render inputLines = do
  lines <- inputLines
  GtkWindow.plotLines lines
  PngImage.plotLines lines 
  return ()

calculate paramsWithoutCoeffs = do
  ReverseMapping.calcLines ModelFunctions.dzdu ModelData.pointlines

input = ()

