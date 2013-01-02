-- This module shadows declarations of calcPoint, calcLine and calcLines
--  with non-pure versions, which will log the processing to the command line.

module ReverseComplexGraph.LoggedCalculation ( 
  calcPoint,
  calcLine,
  calcLines
  ) where

import Text.Printf
import System.CPUTime

import qualified ReverseComplexGraph as PureComputation

calcLines function lines = mapM (calcLine function) lines

calcLine function line = do
  putStrLn $ "Calculating line: " ++ (show line)
  putStrLn $ "Origin point is: " ++ (show origin)

  mapM (calcPoint function origin) line
  where 
    origin = PureComputation.originOf line

calcPoint mapping origin point = do
  starttime <- getCPUTime
  let outPoint = PureComputation.calcPoint mapping origin point
  endtime <- getCPUTime
  let diff = (fromIntegral (endtime - starttime)) / ticksInSecond

  putStrLn $ (show point) ++ " -> " ++ (show outPoint)

  printf "%s -> %s (%0.4f sec)\n" (show point) (show outPoint) (diff :: Double)

  return outPoint
  where
    ticksInSecond = 10 ^ 12

