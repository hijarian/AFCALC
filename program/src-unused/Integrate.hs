module Integrate where
-- Custom integration of complex functions alogn the curve

import Data.Complex

-- Get step between <a> and <b> given number <n> of steps
getQuantizer a b n = (b - a) / (fromInteger n)

-- Uniform quantization of [<a>..<b>] with number of steps equals <n>
quantize a b n = (map ((+ a) . (* h) . fromInteger) [0..(n-2)]) ++ [b]
  where h = getQuantizer a b n

integrate :: (Complex Double -> Complex Double) -> Integer -> Complex Double -> Complex Double -> Complex Double 
integrate f n z1 z2 = (sum $ map f points) * h
  where
    points = zipWith (:+) xpoints ypoints
    xpoints = quantize (realPart z1) (realPart z2) n
    ypoints = quantize (imagPart z1) (imagPart z2) n
    h = getQuantizer z1 z2 n
    
