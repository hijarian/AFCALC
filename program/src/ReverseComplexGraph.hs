module ReverseComplexGraph ( 
  calcPoint,
  calcLine,
  calcLines
  ) where

import Data.Complex

import Data.Complex.Integrate

type Point = (Double, Double)
type PointLine = [Point]
type ComplexPoint = Complex Double
type ComplexPointLine = [Complex Double]
type ComplexFunction = (Complex Double -> Complex Double)

calcLines function lines = map (calcLine function) lines

-- Calculate the line in target area corresponding to the line between
--  given points at source area
calcLine :: ComplexFunction -> ComplexPointLine -> PointLine
calcLine function line = map (calcPoint function origin) line
  where
    origin = line !! 0

-- Calculate only one point of target area
calcPoint :: ComplexFunction -> ComplexPoint -> ComplexPoint -> Point
calcPoint f u0 u = complexAsPoint $ integrate f n u0 u
    where n = 50 -- hardcoded precision

complexAsPoint :: Complex Double -> Point
complexAsPoint u = (realPart u, imagPart u)

