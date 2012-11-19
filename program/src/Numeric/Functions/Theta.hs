-- | Theta-functions implemented on top of trigonometric series.
--   Theta-functions are special functions of several complex variables
--   Their importance is that we can construct an elliptic functions from
--   combination of theta-functions
--   http://en.wikipedia.org/wiki/Theta_function
--   Depend on parameter <tau>, which should be positive
--   Call every function as thetaN <n> (qpar <tau>) <u>
--   where <n> is a number of addends in series representing the function
--   <tau> is a tau parameter defining the theta-function
--   <u> is an argument, which is a complex number

-- | WARNING: theta-functions are raising their values very quickly when arg is raising. 
--   This depends on behaviour of cos and sin of complex functions, 
--   which are very rapidly increasing their values.
--   Call theta-functions with n < 20, q < 1, |u| < pi

module Numeric.Functions.Theta (
    qpar,
    theta1,
    theta2,
    theta3,
    theta4
) where

-- We do not use parallelism yet
-- import Control.Parallel
-- import Control.Parallel.Strategies

-- We work with complex numbers only
import Data.Complex
-- And we have exceptional situations
import Control.Exception

-- | Theta-function depends on parameter q, which abs should be lower than 1
--   Parameter q, however, depends on the main parameter tau, 
--   so we will make q dependent variable
qpar :: RealFloat a => a -> Complex a
qpar tau  
  | tau > 0 = exp $ (pi :+ 0) * (0 :+ tau) * (0 :+ 1)
  | otherwise = throw $ ErrorCall "tau should be > 0 !"

-- | This is an analogue to $ (-1)^n $
signfun :: (RealFloat a) => Integer -> Complex a
signfun nn
  | odd nn = ((-1) :+ 0)
  | otherwise = (1 :+ 0)

-- | Function $ q^n^2 $
qfun1 :: (RealFloat a) => Complex a -> Integer -> Complex a
qfun1 q n = q ** (fromInteger n) ** 2

-- | Function $ q^{n + 1/2}^2 $
qfun2 :: (RealFloat a) => Complex a -> Integer -> Complex a
qfun2 q n = q ** (0.5 + fromInteger n) ** 2

-- | Cosine function tailored for our types
cosfun :: (RealFloat a) => Complex a -> Integer -> Complex a
cosfun u n = cos $ u * fromInteger n 

-- | Sine function tailored for our types
sinfun :: (RealFloat a) => Complex a -> Integer -> Complex a
sinfun u n = sin $ u * fromInteger n

-- | \Theta_1
theta1 :: (RealFloat a) => Integer -> Complex a -> Complex a -> Complex a
theta1 n q u = (* 2) . sum $ map (theta1_arg q u) [0..n]

theta1_arg :: (RealFloat a) => Complex a -> Complex a -> Integer -> Complex a
theta1_arg q u nn =  (signfun nn) * (qfun2 q nn) * (sinfun u (2 * nn + 1))

-- | \Theta_2
theta2 :: (RealFloat a) => Integer -> Complex a -> Complex a -> Complex a
theta2 n q u = (* 2) . sum $ map (theta2_arg q u) [0..n]

theta2_arg :: (RealFloat a) => Complex a -> Complex a -> Integer -> Complex a
theta2_arg q u nn = (qfun2 q nn) * (cosfun u (2 * nn + 1)) 

-- | \Theta_3
theta3 :: (RealFloat a) => Integer -> Complex a -> Complex a -> Complex a
theta3 n q u = (+ 1) . (* 2) . sum $ map (theta3_arg q u) [1..n]

theta3_arg :: (RealFloat a) => Complex a -> Complex a -> Integer -> Complex a
theta3_arg q u nn = (qfun1 q nn) * (cosfun u (2 * nn))

-- | \Theta_4
theta4 :: (RealFloat a) => Integer -> Complex a -> Complex a -> Complex a
theta4 n q u = (+ 1) . (* 2) . sum $ map (theta4_arg q u) [1..n]

theta4_arg :: (RealFloat a) => Complex a -> Complex a -> Integer -> Complex a
theta4_arg q u nn = (signfun nn) * (qfun1 q nn) * (cosfun u (2 * nn))

