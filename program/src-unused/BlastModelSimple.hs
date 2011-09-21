{-
    Модель взрыва заглублённого в грунт заряда криволинейной формы.
    Позволяет вычислить границу воронки взрыва на основании ряда параметров.
    Взрыв представляется как потенциальное течение струи идеальной жидкости, а воронка взрыва --- как линия тока на течении, вдоль которой скорость течения равна некоторому "критическому" значению.
    Сафронов Марк a. k. a. hijarian
    август 2011
    Public Domain
-}
module BlastModelSimple where

-- Мы используем параллельность!
import Control.Parallel.Strategies
import Control.Parallel

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
    a          :: Double,     -- заряд предполагается эллиптическим, поэтому это больший радиус заряда
    b          :: Double,     -- заряд предполагается эллиптическим, поэтому это меньший радиус заряда
    n_theta    :: Integer,    -- количество слагаемых в ряду, представляющем тета-функцию (т. е., это, по сути, точность вычислений тета-функций)
    n_integral :: Integer,    -- частота разбиения отрезка интегрирования
    n_cn       :: Integer,    -- количество коэффициентов cN в разбиении f(u) в ряд, т. е., заодно и точность вычисления f(u)
    precision  :: Double,     -- точность вычисления cN методом простых итераций. Да и вообще "точность" там, где она может быть нужна
    c_n        :: [Double]    -- список коэффициентов cN в разбиении f(u) в ряд. При задании параметров равны начальному приближению, потом уточняются.
-- Не существует c_n !! 0, параметр c0 задаётся в формуле, и здесь не хранится и не обновляется.
    } deriving (Show)

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

-- Параметры "по умолчанию", для упрощения отладки
null_parameters = ModelParams {
    tau        = 0.7,
    phi_0      = 100,
    v_0        = 0.2,
    alpha      = 0.25,
    a          = 2,
    b          = 5,
    n_theta    = 25, -- you'll never need more, 'cause there's an q ** n_theta ** 2 in definition of both theta-functions with q < 1
    n_integral = 50,
    n_cn       = 30,
    precision  = 0.001,
    c_n        = take 30 $ repeat 0
    }

-- Функция для удобства печати коэффициентов cN
printCnList :: ModelParams -> IO()
printCnList param = do
  let cnlist = c_n param
      nlist = [1..(n_cn param)]
      cnnstr = (\(n, cn) -> "n" ++ (show n) ++ ": " ++ (show cn))
  mapM print $ map cnnstr (zip nlist cnlist)
  return ()

-- Более-менее удобное изменение параметров
renew_params param (phi_0', v_0', tau', alpha', a', b') =
  param {phi_0 = phi_0', v_0 = v_0', tau = tau', a = a', b = b'}

-- 0. Различные хелперы для упрощения работы END
----------------------------------------

----------------------------------------
-- 1. Вычисление координат точек на границе воронки взрыва BEGIN

-- Функция для вычисления сразу всех координат
--   получает параметры модели
--   выдаёт список координат точек, пригодных для передачи их на график
zlist :: ModelParams -> [(Double, Double)]
zlist param = map constructPoint [a', a' + h .. b']
  where
    constructPoint e = asPoint $ z param e
    asPoint u = (realPart u, imagPart u)
    h = (b' - a') / fromInteger n'
    a' = precision param
    b' = ( pi * (tau param) ) / 2
    n' = n_integral param

-- Функция для вычисления координат одной точки на границе воронки взрыва
--   получает параметры модели и одну координату точки на границе воронки в поле 'u'
--   выдаёт координаты одной точки в поле 'z'
-- В данной модели интегрирование производится по линии (pi/4) + i*e'
z :: ModelParams -> Double -> Complex Double
z param e = finalIntegrate (dzdu param) lowl e n'
  where n' = n_integral param
        lowl = precision param


-- Функции для получения точек на всех границах области по отдельности
-- Точки на границе CD
zlistCD param = map constructPoint [a', a' + h .. b']
  where
    constructPoint e = asPoint $ zCD param e
    asPoint u = (realPart u, imagPart u)
    h = (b' - a') / fromInteger n'
    a' = precision param
    b' = pi / 4
    n' = n_integral param

zCD param e = integrateCD (dzdu param) lowl e n'
  where n' = n_integral param
        lowl = precision param

-- Точки на границе BA
zlistBA param = map constructPoint [a', a' + h .. b']
  where
    constructPoint e = asPoint $ zBA param e
    asPoint u = (realPart u, imagPart u)
    h = (b' - a') / fromInteger n'
    a' = precision param
    b' = pi / 4
    n' = n_integral param

zBA param e = integrateBA tau' (dzdu param) lowl e n'
  where n'   = n_integral param
        lowl = precision param
        tau' = tau param

-- Точки на границе CB
zlistCB param = map constructPoint [a', a' + h .. b']
  where
    constructPoint e = asPoint $ zCB param e
    asPoint u = (realPart u, imagPart u)
    h = (b' - a') / fromInteger n'
    a' = precision param
    b' = ( pi * (tau param) ) / 4
    n' = n_integral param

zCB param e = integrateCB (dzdu param) lowl e n'
  where n' = n_integral param
        lowl = precision param

-- Точки на границе DA
zlistDA param = map constructPoint [a', a' + h .. b']
  where
    constructPoint e = asPoint $ zDA param e
    asPoint u = (realPart u, imagPart u)
    h = (b' - a') / fromInteger n'
    a' = precision param
    b' = ( pi * (tau param) ) / 4
    n' = n_integral param

zDA param e = integrateDA (dzdu param) lowl e n'
  where n' = n_integral param
        lowl = precision param

-- 1. Вычисление координат точек на границе воронки взрыва END
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
-- dzdu param u = ((dwdu param u) / v_0') * exp ( negate $ chi param u )
--   where v_0' = (v_0 param :+ 0)
dzdu p u = nval' * eval'  * divident' / divisor''
  where
    nval'  = (toComplex (phi_0 p)) * (mfunc p) / toComplex pi / toComplex (v_0 p)
    eval'  = exp ( 0 :+ ((1 - alpha p) * pi)) 
    divident' = t1m4t p u * t1p4t p u * t2m4t p u * t2p4t p u
    divisor'' = (t1m4 p u * t4m4 p u) ** ((1 - alpha p) :+ 0)  * (t1p4 p u * t4p4 p u) ** ((1 + alpha p) :+ 0)

-- 2. Координаты точки на границе воронки взрыва получаем интегрированием dz/du END
----------------------------------------

----------------------------------------
-- 9. Операторы интегрирования BEGIN

-- Оператор интегрирования по ординате. То есть, интегрируем функцию комплексного переменного f(x + iy) по линии (0 + ia) .. (0 + ib).
--   Используется метод трапеций. Вроде как.
--   на вход получаем функцию комплексного аргумента, начальную ординату, конечную ординату и количество отрезков разбиения сетки.
integrateY :: (RealFloat a, Enum a) => (Complex a -> Complex a) -> a -> a -> Integer -> Complex a
integrateY f a b n  =
    ((sum $ map f yvalues) + t) * (h :+ 0)
    where
      values = [a + h * fromInteger(nn) | nn <- [0..n]]
      yvalues = map ((:+) x) values -- DO NOT REMOVE BRACKETS AROUND ':+'
      t = (f (x :+ a) + f (x :+ b))/2
      h = (b - a) / fromInteger(n)
      x = 0

-- Тот же самый integrateY, только интегрирование производится по ((pi/4) + ia) .. ((pi/4) + ib)
finalIntegrate :: (RealFloat a, Enum a) => (Complex a -> Complex a) -> a -> a -> Integer -> Complex a
finalIntegrate f a b n  =
    ((sum $ map f yvalues) + t) * (h :+ 0)
    where
      values = [a + h * fromInteger(nn) | nn <- [0..n]]
      yvalues = map ((:+) x) values -- DO NOT REMOVE BRACKETS AROUND ':+'
      t = (f (x :+ a) + f (x :+ b))/2
      h = (b - a) / fromInteger(n)
      x = pi / 4

integrateCB :: (RealFloat a, Enum a) => (Complex a -> Complex a) -> a -> a -> Integer -> Complex a
integrateCB f a b n  =
    ((sum $ map f yvalues) + t) * (h :+ 0)
    where
      values = [a + h * fromInteger(nn) | nn <- [0..n]]
      yvalues = map ((:+) x) values -- DO NOT REMOVE BRACKETS AROUND ':+'
      t = (f (x :+ a) + f (x :+ b))/2
      h = (b - a) / fromInteger(n)
      x = 0

integrateBA :: (RealFloat a, Enum a) => a -> (Complex a -> Complex a) -> a -> a -> Integer -> Complex a
integrateBA tau' f a b n  =
    ((sum $ map f xvalues) + t) * (h :+ 0)
    where
      values = [a + h * fromInteger(nn) | nn <- [0..n]]
      xvalues = map (:+ y) values
      t = (f (a :+ y) + f (b :+ y))/2
      h = (b - a) / fromInteger(n)
      y = pi * tau' / 4

integrateCD :: (RealFloat a, Enum a) => (Complex a -> Complex a) -> a -> a -> Integer -> Complex a
integrateCD f a b n  =
    ((sum $ map f xvalues) + t) * (h :+ 0)
    where
      values = [a + h * fromInteger(nn) | nn <- [0..n]]
      xvalues = map (:+ y) values
      t = (f (a :+ y) + f (b :+ y))/2
      h = (b - a) / fromInteger(n)
      y = 0

integrateDA :: (RealFloat a, Enum a) => (Complex a -> Complex a) -> a -> a -> Integer -> Complex a
integrateDA f a b n  =
    ((sum $ map f yvalues) + t) * (h :+ 0)
    where
      values = [a + h * fromInteger(nn) | nn <- [0..n]]
      yvalues = map ((:+) x) values -- DO NOT REMOVE BRACKETS AROUND ':+'
      t = (f (x :+ a) + f (x :+ b))/2
      h = (b - a) / fromInteger(n)
      x = pi / 4

-- Оператор интегрирования по абсциссе. То есть, интегрируем функцию комплексного переменного f(x + iy) по линии (a + i0) .. (b + i0).
--   Используется метод трапеций. Вроде как.
--   на вход получаем функцию комплексного аргумента, начальную абсциссу, конечную абсциссу и количество отрезков разбиения сетки.
integrateX :: (RealFloat a, Enum a) => (Complex a -> Complex a) -> a -> a -> Integer -> Complex a
integrateX f a b n  =
    ((sum $ map f xvalues) + t) * (h :+ 0)
    where
      values = [a + h * fromInteger(nn) | nn <- [0..n]]
      xvalues = map (:+ 0) values
      t = (f (a :+ 0) + f (b :+ 0))/2
      h = (b - a) / fromInteger(n)

integrateYreal :: (Double -> Complex Double) -> Double -> Double -> Integer -> Complex Double
integrateYreal f a b n  =
    ((sum $ map f values) + t) * (h :+ 0)
    where
      values = [a + h * fromInteger(nn) | nn <- [0..n]]
      t = (f a + f b) / (2 :+ 0)
      h = (b - a) / fromInteger(n)
      x = 0

-- Оператор интегрирования действительнозначных функций методом трапеций.
-- Получает на вход функцию, нижний предел интегрирования, верхний предел интегрирования и количество отрезков разбиения сетки.
integrate f a b n  =
    ((sum $ map f values) + t) * h
    where
      values = [a + h * fromInteger(nn) | nn <- [0..(n-1)]]
      t = (f a + f b)/2
      h = (b - a) / fromInteger(n)

-- 9. Операторы интегрирования END
----------------------------------------



