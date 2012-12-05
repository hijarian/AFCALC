module Model.Lines (
  pointlines
  ) where

import Model.Params
import Data.Complex

type ComplexPoint = Complex Double
type ComplexPointList = [ComplexPoint]

a, b, c, d :: Double -> Complex Double
a tau = (pi/4) :+ (pi*tau/4)
b tau = 0      :+ (pi*tau/4)
c tau = 0      :+ 0 
d tau = (pi/4) :+ 0 


-- This function should return all lines of points in the auxiliary domain U
--  which should be converted to points in the origin domain Z.
-- Basically, this is the core of ReverseComplexGraph applied to the model.
pointlines :: ModelParams -> [ComplexPointList]
pointlines params = [
  defaultQuantizationOfSegment a b params,
  defaultQuantizationOfSegment b c params,
  defaultQuantizationOfSegment c d params,
  defaultQuantizationOfSegment d a params 
  ]

defaultQuantizationOfSegment :: (Double -> Complex Double) -> (Double -> Complex Double) -> ModelParams -> ComplexPointList
defaultQuantizationOfSegment startPointFunction endPointFunction params =
  quantizeSegment (startPointFunction (tau params)) (endPointFunction (tau params)) (n_integrate params)


quantizeSegment :: ComplexPoint -> ComplexPoint -> Integer -> ComplexPointList
quantizeSegment startPoint endPoint subsegmentsQuantity = map makeSubPoint [0..subsegmentsQuantity]
  where
    makeSubPoint = ((+ startPoint) . (* subsegmentLength) . fromInteger)
    subsegmentLength = (endPoint - startPoint) / (fromInteger subsegmentsQuantity)

