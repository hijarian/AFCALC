{-
Model of explosion of buried curved charge.
Proposed by L. M. Kotlyar in 1970's
Makes possible the calculations of blast edges given set parameters.
Explosion is modelled as potential flow of ideal liquid.

Encoded by Saphronov Mark a. k. a. hijarian
2011.09
Public Domain
-}
module Model.Functions (
  dzdu,
  dzdu',
  coeffCalculationHelper
  ) where

import Data.Complex
import Data.Complex.Integrate

-- We need theta-functions here, 
--  see http://mathworld.wolfram.com/JacobiThetaFunctions.html
import Numeric.Functions.Theta

-- We import the data type named "ModelParams" from there
import Model.Params

type AuxiliaryDomainComplexValue = Complex Double
type OriginDomainComplexValue = Complex Double

-----------------------------------------------------------------------
-- DZDU FUNCTIONS BEGIN
-----------------------------------------------------------------------

-- dz/du
-- That is, differential of the mapping from auxiliary domain U to origin domain Z
dzdu :: ModelParams -> AuxiliaryDomainComplexValue -> OriginDomainComplexValue
dzdu p u = dwdu p u * exp (chi p u)

-- dz/du
-- Provided here for testing and comparing purposes
-- It's a mathematically simplified equivalent of original `dzdu`
dzdu' :: ModelParams -> AuxiliaryDomainComplexValue -> OriginDomainComplexValue
dzdu' p u = (dzdu_simple p u) * (exp $ f_corr p u)

-- Calculates simplified version of the area. It needs correcting function f_corr
dzdu_simple :: ModelParams -> Complex Double -> Complex Double
dzdu_simple p u = nval' * eval'  * divident' / divisor''
  where
    nval'  = (mfunc p) * ((toComplex.negate) $ (phi_0 p) / pi / (v_0 p))
    eval'  = exp ( 0 :+ ((1 - alpha p) * pi))
    divident' = t1m4t p u * t1p4t p u * t2m4t p u * t2p4t p u
    divisor'' = (t1m4 p u * t4m4 p u) ** ((1 - alpha p) :+ 0)  * (t1p4 p u * t4p4 p u) ** ((1 + alpha p) :+ 0)
-----------------------------------------------------------------------
-- DZDU FUNCTIONS END
-----------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Helper which will be passed to the method which renews coefficients
-- It's a function which uses `chi` method, so, we cannot really use this helper
--  right in the module which needs it.
coeffCalculationHelper :: ModelParams -> Double -> Double
coeffCalculationHelper param e = curvature param (t e)
  where
    t e = imagPart $ chi param (0 :+ e)

-- Curvature.
-- Parametrized by the length of the curve, so, has a single argument
--  which is a point at the curve.
-- Returns the curvature at this point
-- TODO: Maybe should convert this function to Complex valued?
curvature :: ModelParams -> Double -> Double
curvature param x = ((1 - epssin) ** (3/2)) / p
  where epssin = (epsilon * (sin x)) ^ 2
        -- Here we see with our own eyes that RAD_B MUST BE GREATER THAN RAD_A!
        epsilon = ( sqrt ( b'*b' - a'*a' ) ) / b'
        p = (a'*a') / (b'*b')
        a' = rad_a param
        b' = rad_b param
--------------------------------------------------------------------------------

-----------------------------------------------------------------------
-- CORE FUNCTIONS BEGIN
-----------------------------------------------------------------------
-- The mapping between points in auxillary
-- domain `u` and source domain `z` is defined as:

-- dzdu param u = ((dwdu param u)) * exp ( negate $ chi param u )

-- and `chi` is `chi_0 - f_corr`
-- So, we need to provide functions dwdu, chi_0 and f_corr

-- Derivative of the complex potential dwdu
dwdu :: ModelParams -> Complex Double -> Complex Double
dwdu p u = npar * (mfunc p) * divident / divisor
  where
    npar = toComplex.negate $ phi_0' / pi
    divident = (t1m4t p u) * (t1p4t p u) * (t2m4t p u) * (t2p4t p u)
    divisor  = (t1m4  p u) * (t1p4  p u) * (t4m4  p u) * (t4p4  p u)
    phi_0' = phi_0 p

-- Small helper for dwdu function
mfunc :: ModelParams -> Complex Double
mfunc param = (* 2) . (** 2) . (/ divisor) $ divident
  where
    divisor = (t2z param) * (t3z param) * (t4z param)
    divident = (** 2) . abs $ t14p4t param

-- Zhukovsky function chi
chi :: ModelParams -> Complex Double -> Complex Double
chi param u = (chi_0 param u) - (f_corr param u)

-- Zhukovsky function chi_0 for simplified problem
chi_0 :: ModelParams -> Complex Double -> Complex Double
chi_0 p u =  (+ cpar).(* (toComplex g)).log $ divident / divisor
  where
    cpar = toImaginary $ (pi * (g - 1))
    g = alpha p
    divident = (t1p4 p u) * (t4p4 p u)
    divisor  = (t1m4 p u) * (t4m4 p u)

-- Correcting function f(u)
-- For it to work, in the ModelParams object there should be already calculated values 
--  of the coefficients `cN` in field `cn`
f_corr :: ModelParams -> Complex Double -> Complex Double
f_corr p u = const_part + (foldl (+) c0 (f_arg u clist))
  where
    const_part    = ((4 * (1 - (gamma/2)) * negate(1/tau') ) :+ 0) * u
    gamma         = alpha p
    c0            = 0 :+ 0 -- :)
    f_arg u clist = map (\ (n, cn) -> ((cn * (1 - rho n)) :+ 0) * exp' n) clist
    clist         = zip [1..(n_cn p)] (c_n p)
    exp' n        = exp $ (4 * u - pi * (fromInteger n)) / (tau' :+ 0)
    rho n         = exp $ (negate (2 * pi * fromInteger n)) / tau'
    tau'          = tau p

-----------------------------------------------------------------------
-- CORE FUNCTIONS END
-----------------------------------------------------------------------

-----------------------------------------------------------------------
-- HELPER FUNCTIONS BEGIN
-----------------------------------------------------------------------

-- Semantically converting to complex
toComplex :: (RealFloat a) => a -> Complex a
toComplex = (:+ 0)

-- Semantically converting to pure imaginary number
-- Equals to multiplying the real number by i
toImaginary :: (RealFloat a) => a -> Complex a
toImaginary = ((:+) 0)

-- We will use theta-functions with parameters from object
--  typed ModelParams, so let's write a helper so we will be able
--  to write just thetaN' <p> <u>
theta1' param = theta1 (n_theta param) (qpar (tau param))
theta2' param = theta2 (n_theta param) (qpar (tau param))
theta3' param = theta3 (n_theta param) (qpar (tau param))
theta4' param = theta4 (n_theta param) (qpar (tau param))

--pi4 :: (RealFloat a) => a
pi4       = pi / 4

--pi4t ::  ModelParams -> Complex Double
pi4t p    = toImaginary . (* pi4) $ (tau p)


-- This is a set of helper functions to quickly calculate
--  some often encountered theta function invocations
t1m4t, t1p4t, t1m4, t1p4 :: ModelParams -> Complex Double -> Complex Double
t1m4t   p u = theta1' p (u - ( pi4t p ))
t1p4t   p u = theta1' p (u + ( pi4t p ))
t1m4    p u = theta1' p (u - (toComplex pi4))
t1p4    p u = theta1' p (u + (toComplex pi4))

t1z ::  ModelParams -> Complex Double
t1z     p   = theta1' p (0 :+ 0)

t14p4t ::  ModelParams -> Complex Double
t14p4t  p   = theta1' p ((toComplex pi4) + ( pi4t p ))

t2m4t, t2p4t, t2m4, t2p4 :: ModelParams -> Complex Double -> Complex Double
t2m4t   p u = theta2' p (u - ( pi4t p ))
t2p4t   p u = theta2' p (u + ( pi4t p ))
t2m4    p u = theta2' p (u - (toComplex pi4))
t2p4    p u = theta2' p (u + (toComplex pi4))

t2z ::  ModelParams -> Complex Double
t2z     p   = theta2' p (0 :+ 0)

t3m4t, t3p4t, t3m4, t3p4 :: ModelParams -> Complex Double -> Complex Double
t3m4t   p u = theta3' p (u - ( pi4t p ))
t3p4t   p u = theta3' p (u + ( pi4t p ))
t3m4    p u = theta3' p (u - (toComplex pi4))
t3p4    p u = theta3' p (u + (toComplex pi4))

t3z ::  ModelParams -> Complex Double
t3z     p   = theta3' p (0 :+ 0)

t4m4t, t4p4t, t4m4, t4p4 :: ModelParams -> Complex Double -> Complex Double
t4m4t   p u = theta4' p (u - ( pi4t p ))
t4p4t   p u = theta4' p (u + ( pi4t p ))
t4m4    p u = theta4' p (u - (toComplex pi4))
t4p4    p u = theta4' p (u + (toComplex pi4))

t4z ::  ModelParams -> Complex Double
t4z     p   = theta4' p (0 :+ 0)

-----------------------------------------------------------------------
-- HELPER FUNCTIONS END
-----------------------------------------------------------------------
