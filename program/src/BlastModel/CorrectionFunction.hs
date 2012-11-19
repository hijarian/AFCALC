{-
Model of explosion of buried curved charge.
Proposed by L. M. Kotlyar in 1970's
Makes possible the calculations of blast edges given set parameters.
Explosion is modelled as potential flow of ideal liquid.

Correction function `f_corr` for both full and manually-simplified `dz/du`

Encoded by Saphronov Mark a. k. a. hijarian
2011.09
Public Domain
-}
module BlastModel.CorrectionFunction where

import BlastModel.ModelParams

-- We use complex functions
import Data.Complex

-- Custom module for integrating complex functions
-- EDIT @ 2011-09-26: Cabalized as complex-integrate package
import Data.Complex.Integrate

-- Custom module for handling theta-functions
-- EDIT @ 2011-09-26: Cabalized as theta-functions package
import Numeric.Functions.Theta

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


-- для того, чтобы корректирующая функция работала, необходимо,
-- чтобы были вычислены коэффициенты cN.

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
