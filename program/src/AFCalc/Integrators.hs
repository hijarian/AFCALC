module AFCalc.Integrators where
-- Integration of complex and real functions along straight lines

-- We will work with complex numbers
import Data.Complex

-- Get step between <a> and <b> given number <n> of steps
getQuantizer :: (Fractional a) => a -> a -> Integer -> a
getQuantizer a b n = (b - a) / (fromInteger n)

-- Integration by Simpson's rule
integrate :: (Fractional v) => (v -> v) -> Integer -> v -> v -> v
integrate f n a b =
    ((f a) + (4 * (f_o)) + (2 * (f_e)) + (f b)) * h / 3
        where
           h = getQuantizer a b n
           f_o = sum $ map (f . (+ a) . (* h) . fromInteger) [nn | nn <- [1..(n-1)], odd nn]
           f_e = sum $ map (f . (+ a) . (* h) . fromInteger) [nn | nn <- [2..(n-2)], even nn]

