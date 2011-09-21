{-
Model of explosion of buried curved charge.
Makes possible the calculations of blast edges given set parameters.
Explosion is modelled as potential flow of ideal liquid.
Saphronov Mark a. k. a. hijarian
2011.09
Public Domain
--
    Модель взрыва заглублённого в грунт заряда криволинейной формы.
    Позволяет вычислить границу воронки взрыва на основании ряда параметров.
    Взрыв представляется как потенциальное течение струи идеальной жидкости, а воронка взрыва --- как линия тока на течении, вдоль которой скорость течения равна некоторому "критическому" значению.
    Сафронов Марк a. k. a. hijarian
    август 2011
    Public Domain
-}

module BlastModel.Simple where

-- Мы используем параллельность!
-- import Control.Parallel.Strategies
-- import Control.Parallel

-- Наша модель построена на комплекснозначных функциях комплексного аргумента
import Data.Complex

-- Импортируем самописные тета-функции
import Theta

-- Параметры модели будем передавать объектом следующего типа:
data ModelParams = ModelParams {
    tau        :: Double,     -- параметр, доопределяет тета-функции
    phi_0      :: Double,     -- начальное значение потенциала течения
    v_0        :: Double,     -- критическое значение скорости, скорость течения равна v_0 на границе воронки взрыва
    alpha      :: Double,     -- угол, с которым граница заряда наклонена к оси абсцисс
    n_theta    :: Integer    -- количество слагаемых в ряду, представляющем тета-функцию (т. е., это, по сути, точность вычислений тета-функций)
    } deriving (Show)

-- Параметры "по умолчанию", для упрощения отладки
null_parameters = ModelParams {
    tau        = 0.6,
    phi_0      = 14,
    v_0        = 0.2,
    alpha      = 0.5,
    n_theta    = 25 -- you'll never need more, 'cause there's an q ** n_theta ** 2 in definition of both theta-functions with q < 1
    }

-- Более-менее удобное изменение параметров
renew_params param (phi_0', v_0', tau', alpha') =
  param {phi_0 = phi_0', v_0 = v_0', tau = tau'}

----------------------------------------
-- 0. Различные хелперы для упрощения работы BEGIN

toComplex :: (RealFloat a) => a -> Complex a
toComplex = (:+ 0)

toImaginary :: (RealFloat a) => a -> Complex a
toImaginary = ((:+) 0)

-- Переобозначения тета-функций в более удобоваримый вид
theta1' param = theta1 (n_theta param) (qpar (tau param))
theta2' param = theta2 (n_theta param) (qpar (tau param))
theta3' param = theta3 (n_theta param) (qpar (tau param))
theta4' param = theta4 (n_theta param) (qpar (tau param))

--pi4 :: (RealFloat a) => a
pi4       = pi / 4

--pi4t ::  ModelParams -> Complex Double
pi4t p    = toImaginary . (* pi4) $ (tau p)

t1m4t   p u = theta1' p (u - ( pi4t p ))
t1p4t   p u = theta1' p (u + ( pi4t p ))
t1m4    p u = theta1' p (u - (toComplex pi4))
t1p4    p u = theta1' p (u + (toComplex pi4))
t1z     p   = theta1' p (0 :+ 0)

--t14p4t :: (RealFloat a) => ModelParams -> Complex a
t14p4t  p   = theta1' p ((toComplex pi4) + ( pi4t p ))

t2m4t   p u = theta2' p (u - ( pi4t p ))
t2p4t   p u = theta2' p (u + ( pi4t p ))
t2m4    p u = theta2' p (u - (toComplex pi4))
t2p4    p u = theta2' p (u + (toComplex pi4))
t2z     p   = theta2' p (0 :+ 0)

t3m4t   p u = theta3' p (u - ( pi4t p ))
t3p4t   p u = theta3' p (u + ( pi4t p ))
t3m4    p u = theta3' p (u - (toComplex pi4))
t3p4    p u = theta3' p (u + (toComplex pi4))
t3z     p   = theta3' p (0 :+ 0)

t4m4t   p u = theta4' p (u - ( pi4t p ))
t4p4t   p u = theta4' p (u + ( pi4t p ))
t4m4    p u = theta4' p (u - (toComplex pi4))
t4p4    p u = theta4' p (u + (toComplex pi4))
t4z     p   = theta4' p (0 :+ 0)


-- 0. Различные хелперы для упрощения работы END
----------------------------------------

----------------------------------------
-- 2. Координаты точки на границе воронки взрыва получаем интегрированием dz/du BEGIN

mfunc :: ModelParams -> Complex Double
mfunc param = (* 2) . (** 2) . (/ divisor) $ divident
  where
    divisor = (t2z param) * (t3z param) * (t4z param)
    divident = (** 2) . abs $ t14p4t param

-- dz/du
dzdu :: ModelParams -> Complex Double -> Complex Double
dzdu p u = nval' * eval'  * divident' / divisor''
  where
    nval'  = (-1)*(toComplex (phi_0 p)) * (mfunc p) / toComplex pi / toComplex (v_0 p)
    eval'  = exp ( 0 :+ ((1 - alpha p) * pi))
    divident' = t1m4t p u * t1p4t p u * t2m4t p u * t2p4t p u
    divisor'' = (t1m4 p u * t4m4 p u) ** ((1 - alpha p) :+ 0)  * (t1p4 p u * t4p4 p u) ** ((1 + alpha p) :+ 0)

-- 2. Координаты точки на границе воронки взрыва получаем интегрированием dz/du END
----------------------------------------