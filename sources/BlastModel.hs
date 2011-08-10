{-
    Модель взрыва заглублённого в грунт заряда криволинейной формы.
    Позволяет вычислить границу воронки взрыва на основании ряда параметров.
    Взрыв представляется как потенциальное течение струи идеальной жидкости, а воронка взрыва --- как линия тока на течении, вдоль которой скорость течения равна некоторому "критическому" значению.
    Сафронов Марк a. k. a. hijarian
    август 2011
    Public Domain
-}
module BlastModel where

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

-- Переобозначения тета-функций в более удобоваримый вид
theta1' param = theta1 (n_theta param) (qpar (0 :+ tau param))
theta2' param = theta2 (n_theta param) (qpar (0 :+ tau param))
theta3' param = theta3 (n_theta param) (qpar (0 :+ tau param))
theta4' param = theta4 (n_theta param) (qpar (0 :+ tau param))

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

-- dz/du
dzdu :: ModelParams -> Complex Double -> Complex Double
dzdu param u = ((dwdu param u) / v_0') * exp ( negate $ chi param u )
  where v_0' = (v_0 param :+ 0)

-- 2. Координаты точки на границе воронки взрыва получаем интегрированием dz/du END
----------------------------------------

----------------------------------------
-- 3. Производная комплексного потенциала BEGIN

-- Производная комплексного потенциала
dwdu :: ModelParams -> Complex Double -> Complex Double
dwdu param u = (npar * mpar * divident) / divisor
  where
    npar = 0 :+ ((phi_0' * 2) / pi)
    mpar = (mdivident / mdivisor) ^ 2
    mdivident = theta2' param 0 * theta3' param 0 * theta4' param 0
    mdivisor = abs $ (theta1' param pA) ^ 2
    pA = (pi/4) :+ (pi * tau' / 4)
    divident = theta1' param pB' * theta1' param pB * theta2' param pB' * theta2' param pB
    divisor  = theta1' param pD' * theta1' param pD * theta4' param pD' * theta4' param pD
    pB  = u + (0 :+ (pi * tau' / 4))
    pB' = u - (0 :+ (pi * tau' / 4))
    pD  = u + ((pi / 4) :+ 0)
    pD' = u - ((pi / 4) :+ 0)
    tau' = tau param
    phi_0' = phi_0 param

-- 3. Производная комплексного потенциала END
----------------------------------------

----------------------------------------
-- 4. Функция Жуковского BEGIN

-- Функция Жуковского
chi :: ModelParams -> Complex Double -> Complex Double
chi param u = (chi_0 param u) - (f_corr param u)

-- 4. Функция Жуковского END
----------------------------------------

----------------------------------------
-- 5. Функция Жуковского для упрощённой задачи BEGIN
-- Функция Жуковского для упрощённой задачи
chi_0 :: ModelParams -> Complex Double -> Complex Double
chi_0 param u = cpar + (gamma :+ 0) * log (divident / divisor)
  where
    cpar = 0 :+ (pi * (gamma - 1))
    gamma = alpha param
    divident = (theta1' param pD ) * (theta4' param pD )
    divisor  = (theta1' param pD') * (theta4' param pD')
    pD  = u + ((pi / 4) :+ 0)
    pD' = u - ((pi / 4) :+ 0)

-- 5. Функция Жуковского для упрощённой задачи END
----------------------------------------

----------------------------------------
-- 6. Корректирующая функция f(u) BEGIN
-- Корректирующая функция f(u)
f_corr :: ModelParams -> Complex Double -> Complex Double
f_corr param u = const_part + (foldl (+) c0 (f_arg u clist))
  where
    const_part    = ((4 * (1 - (gamma/2)) * negate(1/tau') ) :+ 0) * u
    gamma         = alpha param
    c0            = 0 :+ 0 -- :)
    f_arg u clist = map (\ (n, cn) -> ((cn * (1 - rho n)) :+ 0) * exp' n) clist
    clist         = zip [1..(n_cn param)] cn'
    exp' n        = exp $ (4 * u - pi * (fromInteger n)) / (tau' :+ 0)
    rho n         = exp $ (negate (2 * pi * fromInteger n)) / tau'
    cn'           = c_n param
    tau'          = tau param

-- 6. Корректирующая функция f(u) END
----------------------------------------

-- Здесь мы закончили с каркасом для вычисления собственно координат точек. Но для того, чтобы корректирующая функция работала, необходимо, чтобы были вычислены коэффициенты cN.

----------------------------------------
-- 7. Вычисление коэффициентов cN BEGIN

-- Функция для обновления коэффициентов, уже записанных в ModelParams.
-- Она рекурсивная с условием остановки. Основана на методе простых итераций.
renew_cn_all :: ModelParams -> IO (ModelParams)
renew_cn_all param = do
  print "renew_cn_all started"
  new_param <- calc_new_cnlist param
  err <- high_error param new_param
  if err == True
    then renew_cn_all new_param
    else return $ new_param

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
      new_cn = map (calc_cn param) [1..n']
      new_param = param {c_n = new_cn}
  print "new cnlist:"
  printCnList new_param
  return new_param

-- Дополнительная функция для определения того, велика ли ошибка (и надо ли продолжать вычисления)
-- Получает старые ModelParams и новые ModelParams.
-- Возвращает True если ошибка велика и False в противном случае
high_error :: ModelParams -> ModelParams -> IO(Bool)
high_error old_param new_param = do
  let old_cn = c_n old_param
      new_cn = c_n new_param
      errlist = map calc_error (zip old_cn new_cn)
      err = (> precision') $ foldl (+) 0 errlist
-- Вычисляем ошибку
      calc_error (x1, x2)  = ((x2 - x1) ** 2 ) / abs (2 * x1 - x2)
      precision' = precision old_param
  print $ "Error list: " ++ ((show . map (<precision')) errlist)
  print $ "Is error big totally: " ++ (show err)
  return err

-- Функция для вычисления нового коэффициента на основе вектора существующих.
--   принимает объект ModelParams (оттуда берёт вектор cN)
--   принимает номер коэффициента M
--   возвращает число --- уточнённое значение с(M)
calc_cn :: ModelParams -> Integer -> Double
calc_cn param n = (negate divident / divisor) * (ifun param n / ifun param 0)
  where
    divident = 2 * (1 - (gamma / 2))
    divisor  = fromInteger n * (1 + rho)
    rho      = exp $ negate 2 * (pi/tau') * fromInteger n
    tau'     = tau param
    gamma    = alpha param

ifun :: ModelParams -> Integer -> Double
ifun param n = integrate f lowl (pi * tau' / 4) n'
  where
    lowl     = precision param -- Это хак. В нуле функция qfun' обращается в бесконечность (деление на ноль).
    f e      = curvature param (t e) * qfun' param e * cosfun e
    cosfun e = cos $ 4 * fromInteger n * e / tau'
    n'       = n_integral param
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
curvature :: ModelParams -> Double -> Double
curvature param x = ((1 - epssin) ** (3/2)) / p
  where epssin = (epsilon * (sin x)) ^ 2
        epsilon = ( sqrt ( b'*b' - a'*a' ) ) / b'
        p = (a'*a') / (b'*b')
        a' = a param
        b' = b param

-- 8. Функция кривизны END
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




