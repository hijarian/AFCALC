-- Entry point of the whole ReverseComplexGraph program
-- 
module Main where

import qualified ReverseComplexGraph.Canvas.Png as PngRenderer
import qualified ReverseComplexGraph.Canvas.Gtk as GtkRenderer
import qualified ReverseComplexGraph.Model.Functions as ModelFunctions
import qualified ReverseComplexGraph.Model.CorrectionFunctionCoefficients as CoeffCalc
import qualified ReverseComplexGraph.Model.Params as ModelParams

import Data.Complex

main = do

  -- NOTE: this is preliminary testing calls
  calculate $ input
  render 

  -- TODO:  correct incantation is this: 
  -- render $ calculate $ input

  return ()

render = do
  let
    lines = [
      [ (6, 6), (6, 1), (6, 2), (6, 3)],
      [ (1, 6), (2, 6), (3, 6), (4, 6)]
      ]
  GtkRenderer.plotLines lines
  PngRenderer.plotLines lines 
  return ()

calculate params = do
  paramsWithCn <- CoeffCalc.renewCn params
  let
    u = (pi/4 :+ pi/8)
    z = ModelFunctions.dzdu paramsWithCn u
    z'= ModelFunctions.dzdu' paramsWithCn u
  print $ "U = " ++ (show u)
  print $ "Z = " ++ (show z)  ++ " (full model)"
  print $ "Z = " ++ (show z') ++ " (simplified model)"
  return ()

input = ModelParams.defaults

