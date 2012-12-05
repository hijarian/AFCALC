-- Entry point of the whole ReverseComplexGraph program
-- 
module Main where

import qualified ReverseComplexGraph as ReverseMapping

import qualified ReverseComplexGraph.Canvas.Png as PngImage
import qualified ReverseComplexGraph.Canvas.Gtk as GtkWindow

import qualified Model.Functions as ModelFunctions
import qualified Model.CorrectionFunctionCoefficients as Coefficients
import qualified Model.Params as ModelParams
import qualified Model.Lines as ModelData

import Data.Complex

main = do
  render $ calculate $ input
  return ()

render inputLines = do
  lines <- inputLines
  GtkWindow.plotLines lines
  PngImage.plotLines lines 
  return ()

calculate paramsWithoutCoeffs = do
  params <- Coefficients.renew paramsWithoutCoeffs
  let
    points = ReverseMapping.calcLines (ModelFunctions.dzdu params) (ModelData.pointlines params)
  return points

input = ModelParams.defaults

