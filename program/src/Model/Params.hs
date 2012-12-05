{-
Model of explosion of buried curved charge.
Proposed by L. M. Kotlyar in 1970's
Makes possible the calculations of blast edges given set parameters.
Explosion is modelled as potential flow of ideal liquid.

Definition of the record with model parameters.

Encoded by Saphronov Mark a. k. a. hijarian
2011.09
Public Domain
-}
module Model.Params ( 
  defaults,
  tau,
  phi_0,
  v_0,
  alpha,
  rad_a,
  rad_b,
  n_theta,
  n_cn,
  n_integrate,
  c_n,
  ModelParams
  ) where

-- Model parameters are defined as such:
data ModelParams = ModelParams {
  -- tau parameter used in Theta functions
  --  and defines the height of area of U variable.
  tau         :: Double,

  -- starting value of potential of the 'flow'.
  phi_0       :: Double,

  -- critical value of speed of flow. Speed v = v_0 at the edge of blast.
  v_0         :: Double,

  -- pi*alpha/2 is an angle between surface and the edge of explosive charge
  alpha       :: Double,

  -- small radius of elliptical edge of explosive charge
  rad_a       :: Double,

  -- large radius of elliptical edge of explosive charge
  -- it really should be larger than rad_a, or else we get NaN in curvature
  rad_b       :: Double,

  -- number of addends in the series representing the Theta function
  --   (essentially this is a precision of Theta function value computations)
  -- DO NOT set it higher than 20-25, you'll get divergent series!
  n_theta     :: Integer,

  -- number of coefficients Cn (essentially precision of computations of f(u),
  --   but do not set it to values higher than 20-25, you can get
  --   divergent series because of calculation errors)
  n_cn        :: Integer,

  -- number of subsegments to make when calculating the integrals
  -- NOTE: it affects the number of points on lines in model!
  n_integrate :: Integer,

  -- coefficients Cn used in definition of f(u) function.
  -- Equals to some default values at the beginning.
  -- We need to calculate them first, and only after that run calculations
  --   of dzdu
  -- Value of c_n !! 0 does not exist. Cn(0) is set in the formulas of model
  --   and doesn't renew itself
  c_n         :: [Double]
  } deriving (Show)

-- Default parameters, useful for quick runs in ghci
defaults = ModelParams {
    tau         = 0.4,
    phi_0       = 1,
    v_0         = 1,
    alpha       = 1.3,
    rad_a       = 3, -- radius A of elliptical form of the explosive charge
    rad_b       = 5, -- radius B of elliptical form of the explosive charge
    n_theta     = 25, -- you'll never need more, 'cause there's an q ** n_theta ** 2 in definition of both theta-functions with q < 1
    n_cn        = 25,
    n_integrate = 12, -- you'll probably will not need more anyway
    c_n         = take 25 $ repeat 0
    }

