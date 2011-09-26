{-
Model of explosion of buried curved charge.
Proposed by L. M. Kotlyar in 1970's
Makes possible the calculations of blast edges given set parameters.
Explosion is modelled as potential flow of ideal liquid.

Encoded by Saphronov Mark a. k. a. hijarian
2011.09
Public Domain
-}
module BlastModel.Model_1975_Kotlyar where

-- We do not use parallelism yet
--import Control.Parallel.Strategies
--import Control.Parallel

-- We use complex functions
import Data.Complex

-- Custom module for integrating complex functions
-- EDIT @ 2011-09-26: Cabalized as complex-integrate package
import Data.Complex.Integrate

-- Custom module for handling theta-functions
-- EDIT @ 2011-09-26: Cabalized as theta-functions package
import Numeric.Functions.Theta


-- We will pretty-print some of values
import Text.Printf

-----------------------------------------------------------------------
-- MODEL PARAMETERS BEGIN 
-----------------------------------------------------------------------
-- Model parameters are defined as such:
data ModelParams = ModelParams {
    -- tau parameter used in Theta functions 
    --   and defines the height of area of U variable
    tau        :: Double,     
    -- starting value of potential of the 'flow'
    phi_0      :: Double,     
    -- critical value of speed of flow. Speed v = v_0 at the edge of blast
    v_0        :: Double,
    -- pi*alpha/2 is an angle between CD (surface) and AD 
    --   (explosive charge edge)
    alpha      :: Double,
    -- small radius of elliptical edge of explosive charge
    rad_a      :: Double,
    -- large radius of elliptical edge of explosive charge
    -- it really should be larger than rad_a, or else we get NaN in curvature
    rad_b      :: Double,
    -- number of addends in the series representing the Theta function 
    --   (essentially this is a precision of Theta function value computations)
    -- DO NOT set it higher than 20-25, you'll get divergent series!
    n_theta    :: Integer,
    -- number of coefficients Cn (essentially precision of computations of f(u),
    --   but do not set it to values higher than 20-25, you can get 
    --   divergent series because of calculation errors)
    n_cn       :: Integer,
    -- coefficients Cn used in definition of f(u) function.
    -- Equals to some default values at the beginning.
    -- We need to calculate them first, and only after that run calculations 
    --   of dzdu
    -- Value of c_n !! 0 does not exist. Cn(0) is set in the formulas of model
    --   and doesn't renew itself
    c_n        :: CnList
    } deriving (Show)

-- Type of list of coefficients for correcting functions
type CnList = [Double]

-- Default parameters, useful for quick runs in ghci
model_defaults = ModelParams {
    tau        = 0.4,
    phi_0      = 1,
    v_0        = 1,
    alpha      = 1.3,
    rad_a      = 3, -- radius A of elliptical form of the explosive charge
    rad_b      = 5, -- radius B of elliptical form of the explosive charge
    n_theta    = 25, -- you'll never need more, 'cause there's an q ** n_theta ** 2 in definition of both theta-functions with q < 1
    n_cn       = 25,
    c_n        = take 25 $ repeat 0
    }
-----------------------------------------------------------------------
-- MODEL PARAMETERS END
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

-- Function for pretty-printing the Cn list
printCnList :: ModelParams -> IO()
printCnList param = do
  let cnlist = c_n param
      nlist = [1..(n_cn param)]
  mapM printCn (zip nlist cnlist)
  return ()

printCn :: (Integer, Double) -> IO()
printCn (n, cn) = do
  printf "n%3d: %8.3f\n" n cn
  return ()
  
-----------------------------------------------------------------------
-- HELPER FUNCTIONS END
-----------------------------------------------------------------------

-----------------------------------------------------------------------
-- CORE FUNCTIONS BEGIN
-----------------------------------------------------------------------
-- In AFCalc following function is defined:
--dzdu param u = ((dwdu param u)) * exp ( negate $ chi param u )
-- So, we need to provide only functions dwdu, chi_0, f_corr

-- Small helper for dwdu function
mfunc :: ModelParams -> Complex Double
mfunc param = (* 2) . (** 2) . (/ divisor) $ divident
  where
    divisor = (t2z param) * (t3z param) * (t4z param)
    divident = (** 2) . abs $ t14p4t param

-- Derivative of the complex potential dwdu
dwdu :: ModelParams -> Complex Double -> Complex Double
dwdu p u = npar * (mfunc p) * divident / divisor
  where
    npar = toComplex.negate $ phi_0' / pi
    divident = (t1m4t p u) * (t1p4t p u) * (t2m4t p u) * (t2p4t p u)
    divisor  = (t1m4  p u) * (t1p4  p u) * (t4m4  p u) * (t4p4  p u)
    phi_0' = phi_0 p

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

-- 6. Корректирующая функция f(u) END
----------------------------------------

-- Здесь мы закончили с каркасом для вычисления собственно координат точек. Но для того, чтобы корректирующая функция работала, необходимо, чтобы были вычислены коэффициенты cN.

----------------------------------------
-- 7. Вычисление коэффициентов cN BEGIN

-- Дополнительная функция для вычисления новых коэффициентов.
-- Получает текущие ModelParams
-- Возвращает обновлённые ModelParams
calc_new_cnlist :: ModelParams -> IO (ModelParams)
calc_new_cnlist param = do
  print "calc_new_cnlist started"
  print "current cnlist:"
  printCnList param
-- Вычисляем новые параметры
  let n' = n_cn param
      new_cn = map (calc_cn 0.0001 param) [1..n']
      new_param = param {c_n = new_cn}
  print "new cnlist:"
  printCnList new_param
  return new_param

-- Дополнительная функция для определения того, велика ли ошибка (и надо ли продолжать вычисления)
-- Получает старые ModelParams и новые ModelParams.
-- Возвращает True если ошибка велика и False в противном случае
high_error :: Double -> ModelParams -> ModelParams -> IO(Bool)
high_error precision old_param new_param = do
  let old_cn = c_n old_param
      new_cn = c_n new_param
      -- Вычисляем ошибку
      calc_error (x1, x2)  = ((x2 - x1) ** 2 ) / abs (2 * x1 - x2)
      errlist = map calc_error (zip old_cn new_cn)
      err = (> precision) $ foldl (+) 0 errlist
  print $ "Error list: " ++ ((show . map (< precision)) errlist)
  print $ "Is error big totally: " ++ (show err)
  return err

-- Функция для вычисления нового коэффициента на основе вектора существующих.
--   принимает объект ModelParams (оттуда берёт вектор cN)
--   принимает номер коэффициента M
--   возвращает число --- уточнённое значение с(M)
calc_cn :: Double -> ModelParams -> Integer -> Double
calc_cn precision p n = (negate divident / divisor) * (ifun precision p n / ifun precision p 0)
  where
    divident = 2 * (1 - gamma)
    divisor  = fromInteger n * (1 + rho)
    rho      = exp $ negate 2 * (pi/tau') * fromInteger n
    tau'     = tau p
    gamma    = (/ 2) $ alpha p

ifun :: Double -> ModelParams -> Integer -> Double
ifun precision param n = integrate f n' zero (pi * tau' / 4)
  where
    zero     = precision -- Это хак. В нуле функция qfun' обращается в бесконечность (деление на ноль).
    f e      = curvature param (t e) * qfun' param e * cosfun e
    cosfun e = cos $ 4 * fromInteger n * e / tau'
    n'       = 20 -- TODO: rewrite ifun to accept generic parameter
    tau'     = tau param
    t e      = imagPart (chi param (0 :+ e))

qfun' :: ModelParams -> Double -> Double
qfun' param e = ((divident ^ 2) * (efun param e)) / (divisor1 * divisor2)
  where
    divident = (realPart . abs) $ theta2' param eA * theta2' param eA'
    divisor1 = ((** gamma1) . realPart . abs) $ (theta2' param ie) * (theta3' param ie)
    divisor2 = ((** gamma2) . realPart . abs) $ (theta1' param ie) * (theta4' param ie)
    eA  = ((pi / 4) :+ (e + (pi * tau' / 4)))
    eA' = ((pi / 4) :+ (e - (pi * tau' / 4)))
    ie  = (0 :+ e)
    tau' = tau param
    gamma1 = (1 + alpha param)
    gamma2 = (1 - alpha param)

efun :: ModelParams -> Double -> Double
efun param e = (exp . sum) $ map (transform e) cnlist
  where
    transform e (n, cn) = cn * (1 - exparg n) * cosarg n * e
    exparg n = exp $ negate 2 * pi * fromInteger n / tau'
    cosarg n = cos $ 4 * fromInteger n / tau'
    cnlist = zip [1..n'] cn'
    cn' = tail $ c_n param
    n'  = n_cn param
    tau' = tau param

-- 7. Вычисление коэффициентов cN END
----------------------------------------

----------------------------------------
-- 8. Функция кривизны BEGIN

-- Определена как кривизна в точках, расположенных вдоль кривой.
-- Т. о., параметризована длиной кривой, и имеет один аргумент.
--  получает параметр указывающий на точку на криволинейной дуге,
--  выдаёт значение кривизны в этой точке
-- TODO: Maybe should convert this function to Complex valued?
curvature :: ModelParams -> Double -> Double
curvature param x = ((1 - epssin) ** (3/2)) / p
  where epssin = (epsilon * (sin x)) ^ 2
        -- Here we see with our own eyes that RAD_B MUST BE GREATER THAN RAD_A!
        epsilon = ( sqrt ( b'*b' - a'*a' ) ) / b'
        p = (a'*a') / (b'*b')
        a' = rad_a param
        b' = rad_b param

-- 8. Функция кривизны END
----------------------------------------

-----------------------------------------------------------------------
-- CORE FUNCTIONS END
-----------------------------------------------------------------------

-----------------------------------------------------------------------
-- DZDU FUNCTIONS BEGIN
-----------------------------------------------------------------------
-- Additionally, including here the complete dzdu' function
--   it is inferred in Kotlyar's work and is useful for reference

-- dz/du
-- Provided here for testing and comparing purposes
-- It's a mathematically simplified equivalent of combination 
--   dzdu = dwdu * exp (f_corr - chi_0)
-- AFCalc already provides function named dzdu so we use quoted version
dzdu :: ModelParams -> Complex Double -> Complex Double
dzdu p u = (dzdu_simple p u) * (exp $ f_corr p u)

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

