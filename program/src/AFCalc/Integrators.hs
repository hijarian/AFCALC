module AFCalc.Integrators where
-- Different variations of integration of complex functions
-- along straight lines.

import Data.Complex

-- Get step between <a> and <b> given number <n> of steps
getQuantizer :: Double -> Double -> Integer -> Double 
getQuantizer a b n = (b - a) / (fromInteger n)

-- Uniform quantization of [<a>..<b>] with number of steps equals <n>
quantize :: Double -> Double -> Integer -> [Double]
quantize a b n = (map ((+ a) . (* h) . fromInteger) [0..(n-2)]) ++ [b]
  where h = getQuantizer a b n

-- Make list of complex points with ordinate == y
-- (Points will effectively be at the line parallel to the 0x)
-- \xvalues - List of abscissa values
-- return value - list of complex numbers
makeXPoints :: [Double] -> Double -> [Complex Double]
makeXPoints xvalues y = map (:+ y) xvalues

-- Make list of complex points with abscissa == x
-- (Points will effectively be at the line parallel to the 0y)
-- \yvalues - List of ordinate values
-- return value - list of complex numbers
makeYPoints :: [Double] -> Double -> [Complex Double]
makeYPoints yvalues x = map ((:+) x) yvalues -- DO NOT REMOVE BRACKETS AROUND ':+'


-- Integrate complex function <f> along the straight line 
--  between points (<x>, <ya>) and (<x>, <yb>)
-- <n> is a discretization coefficient
integrateVertical :: (Complex Double -> Complex Double) -> Integer -> Double -> Double -> Double -> Complex Double 
integrateVertical f n ya yb x =
    (sum $ map f ypoints) * (h :+ 0)
    where
      ypoints = makeYPoints yvalues x
      yvalues = quantize ya yb n
      h = getQuantizer ya yb n
  
-- Integrate complex function <f> along the straight line 
--  between points (<xa>, <y>) and (<xb>, <y>)
-- <n> is a discretization coefficient
integrateHorizontal :: (Complex Double -> Complex Double) -> Integer -> Double -> Double -> Double -> Complex Double
integrateHorizontal f n xa xb y =
    (sum $ map f xpoints) * (h :+ 0)
    where
      xpoints = makeYPoints xvalues y
      xvalues = quantize xa xb n
      h = getQuantizer xa xb n

-- Integrate real function of one variable using method of trapezies
-- That same method is used in integrateHorizontal and integrateVertical
integrateReal :: (Double -> Double) -> Integer -> Double -> Double -> Double
integrateReal f n xa xb =
    ((sum $ map f xpoints) + t) * h 
    where
      xpoints = quantize xa xb n
      t = ((f xa) + (f xb)) / 2
      h = getQuantizer xa xb n

