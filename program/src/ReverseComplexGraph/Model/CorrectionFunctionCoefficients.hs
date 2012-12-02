{-
Model of explosion of buried curved charge.
Proposed by L. M. Kotlyar in 1970's
Makes possible the calculations of blast edges given set parameters.
Explosion is modelled as potential flow of ideal liquid.

Method to calculate the `cN` coefficients in the model params.
This coefficients are the coefficients needed to calculate the correction function `f_corr`
used in the `ReverseComplexGraph.Model.Functions` module
This coeffients should be already calculated and saved in the model parameters
before you can safely use the `dzdu` function from that module!

Encoded by Saphronov Mark a. k. a. hijarian
2011.09
Public Domain
-}
module ReverseComplexGraph.Model.CorrectionFunctionCoefficients ( 
  renewCn
  ) where

import ReverseComplexGraph.Model.Params
import qualified ReverseComplexGraph.Model.Functions as ModelFunctions

-- We use complex functions
import Data.Complex

import Data.Complex.Integrate

import Numeric.Functions.Theta

-- We will pretty-print some of values
-- TODO: get rid of this horror
import Text.Printf

-- We use custom-built timing function
import ReverseComplexGraph.Time

type Precision = Double
type ParametrizedRealFunction = (ModelParams -> Double -> Double)



--  Renew Cn parameters needed for full model
-- As input accepting old model parameters
-- Returns renewed parameters containing changed Cn list
renewCn :: ModelParams -> IO (ModelParams)
renewCn params = do
  putStrLn "Second, we compute the parameters c_n needed for computations"
  -- We use Fourier method for computations
  new_params <- time $ renew_cn_fourier params 0.0001 ModelFunctions.coeffCalculationHelper
  return new_params

-- Renew Cn list, saved in mpar
-- Returns mpar with new Cn list
-- Based on Fourier method of simple iterations
renew_cn_fourier :: ModelParams -> Precision -> ParametrizedRealFunction -> IO (ModelParams)
renew_cn_fourier mpar precision calc_helper = do
  print "renew_cn_all started"
  new_mpar <- calc_new_cnlist mpar precision calc_helper
  err <- high_error precision mpar new_mpar
  if err == True
    then renew_cn_fourier new_mpar precision calc_helper
    else return $ new_mpar

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

-- Дополнительная функция для вычисления новых коэффициентов.
-- Получает текущие ModelParams
-- Возвращает обновлённые ModelParams
calc_new_cnlist :: ModelParams -> Precision -> ParametrizedRealFunction -> IO (ModelParams)
calc_new_cnlist param precision calc_helper = do
  print "calc_new_cnlist started"
  print "current cnlist:"
  printCnList param
-- Вычисляем новые параметры
  let n' = n_cn param
      new_cn = map (calc_cn param precision calc_helper) [1..n']
      new_param = param {c_n = new_cn}
  print "new cnlist:"
  printCnList new_param
  return new_param

-- From here onwards only the calculations

-- Функция для вычисления нового коэффициента на основе вектора существующих.
--   принимает объект ModelParams (оттуда берёт вектор cN)
--   принимает номер коэффициента M
--   возвращает число --- уточнённое значение с(M)
calc_cn :: ModelParams -> Precision -> ParametrizedRealFunction -> Integer -> Double
calc_cn p precision calc_helper n = 
  (negate divident / divisor) * (ifun p n precision calc_helper / ifun p 0 precision calc_helper)
  where
    divident = 2 * (1 - gamma)
    divisor  = fromInteger n * (1 + rho)
    rho      = exp $ negate 2 * (pi/tau') * fromInteger n
    tau'     = tau p
    gamma    = (/ 2) $ alpha p

-- This is a bloated part of the calculation.
-- Bloat here is because of `precision` and `calc_helper` params
-- `precision` is needed because we cannot really start integrating right from zero
--   or else `qfun` later will break because of division by zero
-- `calc_helper` is needed because it's a curvature of the point which is
--   calculated by applying `chi` function to real value. And `chi` function 
--   is defined in different module. So, instead of creating a dependency
--   I decided to use additional parameter
ifun :: ModelParams -> Integer -> Precision -> ParametrizedRealFunction -> Double
ifun param n precision calc_helper = integrate f n' precision (pi * tau' / 4)
  where
    cosfun e = cos $ 4 * fromInteger n * e / tau'
    f e      = calc_helper param e * qfun param e * cosfun e
    n'       = 20 -- TODO: rewrite ifun to accept generic parameter
    tau'     = tau param

qfun :: ModelParams -> Double -> Double
qfun param e = ((divident ^ 2) * (efun param e)) / (divisor1 * divisor2)
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


-----------------------------------------------------------------------
-- HELPER FUNCTIONS BEGIN
-----------------------------------------------------------------------

-- We will use theta-functions with parameters from object
--  typed ModelParams, so let's write a helper so we will be able
--  to write just thetaN' <p> <u>
theta1' param = theta1 (n_theta param) (qpar (tau param))
theta2' param = theta2 (n_theta param) (qpar (tau param))
theta3' param = theta3 (n_theta param) (qpar (tau param))
theta4' param = theta4 (n_theta param) (qpar (tau param))

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
