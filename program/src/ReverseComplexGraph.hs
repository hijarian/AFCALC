module ReverseComplexGraph ( 
  calcPoint,
  calcLine,
  calcLines,
  originOf
  ) where

import Data.Complex

import Data.Complex.Integrate

type Point = (Double, Double)
type PointLine = [Point]
type ComplexPoint = Complex Double
type ComplexPointLine = [Complex Double]
type ComplexFunction = (Complex Double -> Complex Double)

defaultIntegrationPrecision = 12

calcLines function lines = map (calcLine function) lines

-- Calculate the line in target area corresponding to the line between
--  given points at source area
calcLine :: ComplexFunction -> ComplexPointLine -> PointLine
calcLine function line = map (calcPoint function origin) line
  where origin = originOf line

-- Calculate only one point of target area
calcPoint :: ComplexFunction -> ComplexPoint -> ComplexPoint -> Point
calcPoint mapping origin point = complexAsPoint $ integrate mapping defaultIntegrationPrecision origin point

complexAsPoint :: Complex Double -> Point
complexAsPoint u = (realPart u, imagPart u)

-- Point from which to start the integration
-- To be dead simple, it just uses first point on line as the base
originOf line = line !! 0
