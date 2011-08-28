-- Uniform quantization of [<a>..<b>] with number of steps equals <n>
-- We are doing simple hack here to *always* get last and first
-- elements of quantization equal <a> and <b>, respectively
-- Or else we can get rounding errors when doing (* n) . (/ n) $ b - a
-- uniQuantize :: (RealFloat c, Integral n) => c -> c -> n -> [c] 
-- uniQuantize a b n = ( map ((+ a) . (* h)) [0..(n-1)] ) ++ [b*h] 
--   where h = (b - a) / n

-- 9. Операторы интегрирования END
----------------------------------------

-- Points at line CD
-- That's between (0 :+ 0) and (pi/4 :+ 0)
-- zlistCD param = map constructPoint $ quantize a' b' n'
--   where
--     constructPoint = asPoint . (zCD param)
--     a' = precision param
--     b' = pi / 4
--     n' = n_integral param

-- We calculate coordinates of points as integrals
-- along lines.
-- Point z=(<zx>,0) at line CD is calculated as integral from
-- (0, 0) to (<zx>,0).
-- We start not at x=0, but at x=precision, because of rounding errors
-- We give RealFloat <zx> not the Complex <z> because integrateHorizontal
-- will make Complex values itself
-- zCD param zx = integrateHorizontal (dzdu param) n' zero zx 0 
--   where n' = n_integral param
--         zero = precision param

-- zCD param e = integrateCD (dzdu param) lowl e n'
--   where n' = n_integral param
--         lowl = precision param

-- Точки на границе BA
-- zlistBA param = map constructPoint $ quantize a' b' n'
--   where
--     constructPoint = asPoint . (zBA param) 
--     a' = precision param
--     b' = pi / 4
--     n' = n_integral param

-- zBA param e = integrateBA tau' (dzdu param) lowl e n'
--   where n'   = n_integral param
--         lowl = precision param
--         tau' = tau param

-- -- Точки на границе CB
-- zlistCB param = map constructPoint $ quantize a' b' n'
--   where
--     constructPoint e = asPoint $ zCB param e
--     a' = precision param
--     b' = ( pi * (tau param) ) / 4
--     n' = n_integral param

-- zCB param e = integrateCB (dzdu param) lowl e n'
--   where n' = n_integral param
--         lowl = precision param

-- -- Точки на границе DA
-- zlistDA param = map constructPoint $ quantize a' b' n'
--   where
--     constructPoint e = asPoint $ zDA param e
--     a' = precision param
--     b' = ( pi * (tau param) ) / 4
--     n' = n_integral param

-- zDA param e = integrateDA (dzdu param) lowl e n'
--   where n' = n_integral param
--         lowl = precision param

-- integrateCB :: (RealFloat a, Enum a) => (Complex a -> Complex a) -> a -> a -> Integer -> Complex a
-- integrateCB f a b n  =
--     ((sum $ map f yvalues) + t) * (h :+ 0)
--     where
--       xvalues = [a + h * fromInteger(nn) | nn <- [0..n]]
--       ypoints = makeYPoints xvalues x 
--       t = (f (x :+ a) + f (x :+ b))/2
--       h = (b - a) / fromInteger(n)
--       x = 0

-- integrateBA :: (RealFloat a, Enum a) => a -> (Complex a -> Complex a) -> a -> a -> Integer -> Complex a
-- integrateBA tau' f a b n  =
--     ((sum $ map f xvalues) + t) * (h :+ 0)
--     where
--       values = [a + h * fromInteger(nn) | nn <- [0..n]]
--       xvalues = map (:+ y) values
--       t = (f (a :+ y) + f (b :+ y))/2
--       h = (b - a) / fromInteger(n)
--       y = pi * tau' / 4

-- integrateCD :: (RealFloat a, Enum a) => (Complex a -> Complex a) -> a -> a -> Integer -> Complex a
-- integrateCD f a b n  =
--     ((sum $ map f xvalues) + t) * (h :+ 0)
--     where
--       values = [a + h * fromInteger(nn) | nn <- [0..n]]
--       xvalues = map (:+ y) values
--       t = (f (a :+ y) + f (b :+ y))/2
--       h = (b - a) / fromInteger(n)
--       y = 0

-- integrateDA :: (RealFloat a, Enum a) => (Complex a -> Complex a) -> a -> a -> Integer -> Complex a
-- integrateDA f a b n  =
--     ((sum $ map f yvalues) + t) * (h :+ 0)
--     where
--       values = [a + h * fromInteger(nn) | nn <- [0..n]]
--       yvalues = map ((:+) x) values -- DO NOT REMOVE BRACKETS AROUND ':+'
--       t = (f (x :+ a) + f (x :+ b))/2
--       h = (b - a) / fromInteger(n)
--       x = pi / 4

-- Оператор интегрирования по абсциссе. То есть, интегрируем функцию комплексного переменного f(x + iy) по линии (a + i0) .. (b + i0).
--   Используется метод трапеций. Вроде как.
--   на вход получаем функцию комплексного аргумента, начальную абсциссу, конечную абсциссу и количество отрезков разбиения сетки.
-- integrateX :: (RealFloat a, Enum a) => (Complex a -> Complex a) -> a -> a -> Integer -> Complex a
-- integrateX f a b n  =
--     ((sum $ map f xvalues) + t) * (h :+ 0)
--     where
--       values = [a + h * fromInteger(nn) | nn <- [0..n]]
--       xvalues = map (:+ 0) values
--       t = (f (a :+ 0) + f (b :+ 0))/2
--       h = (b - a) / fromInteger(n)

-- integrateYreal :: (Double -> Complex Double) -> Double -> Double -> Integer -> Complex Double
-- integrateYreal f a b n  =
--     ((sum $ map f values) + t) * (h :+ 0)
--     where
--       values = [a + h * fromInteger(nn) | nn <- [0..n]]
--       t = (f a + f b) / (2 :+ 0)
--       h = (b - a) / fromInteger(n)
--       x = 0

-- Функция для вычисления сразу всех координат
--   получает параметры модели
--   выдаёт список координат точек, пригодных для передачи их на график
-- zlist :: ModelParams -> [(Double, Double)]
-- zlist param = map constructPoint [a', a' + h .. b']
--   where
--     constructPoint e = asPoint $ z param e
--     asPoint u = (realPart u, imagPart u)
--     h = (b' - a') / fromInteger n'
--     a' = precision param
--     b' = ( pi * (tau param) ) / 2
--     n' = n_integral param

-- Функция для вычисления координат одной точки на границе воронки взрыва
--   получает параметры модели и одну координату точки на границе воронки в поле 'u'
--   выдаёт координаты одной точки в поле 'z'
-- В данной модели интегрирование производится по линии (pi/4) + i*e'
-- z :: ModelParams -> Double -> Complex Double
-- z param e = finalIntegrate (dzdu param) lowl e n'
--   where n' = n_integral param
--         lowl = precision param

-- Оператор интегрирования действительнозначных функций методом трапеций.
-- Получает на вход функцию, нижний предел интегрирования, верхний предел интегрирования и количество отрезков разбиения сетки.
-- integrate f a b n  =
--     ((sum $ map f values) + t) * h
--     where
--       values = [a + h * fromInteger(nn) | nn <- [0..(n-1)]]
--       t = (f a + f b)/2
--       h = (b - a) / fromInteger(n)

-- Оператор интегрирования по ординате. То есть, интегрируем функцию комплексного переменного f(x + iy) по линии (0 + ia) .. (0 + ib).
--   Используется метод трапеций. Вроде как.
--   на вход получаем функцию комплексного аргумента, начальную ординату, конечную ординату и количество отрезков разбиения сетки.
-- integrateY :: (RealFloat a, Enum a) => (Complex a -> Complex a) -> a -> a -> Integer -> Complex a
-- integrateY f a b n  =
--     ((sum $ map f yvalues) + t) * (h :+ 0)
--     where
--       values = [a + h * fromInteger(nn) | nn <- [0..n]]
--       yvalues = map ((:+) x) values -- DO NOT REMOVE BRACKETS AROUND ':+'
--       t = (f (x :+ a) + f (x :+ b))/2
--       h = (b - a) / fromInteger(n)
--       x = 0

-- Тот же самый integrateY, только интегрирование производится по ((pi/4) + ia) .. ((pi/4) + ib)
-- finalIntegrate :: (RealFloat a, Enum a) => (Complex a -> Complex a) -> a -> a -> Integer -> Complex a
-- finalIntegrate f a b n  =
--     ((sum $ map f yvalues) + t) * (h :+ 0)
--     where
--       values = [a + h * fromInteger(nn) | nn <- [0..n]]
--       yvalues = map ((:+) x) values -- DO NOT REMOVE BRACKETS AROUND ':+'
--       t = (f (x :+ a) + f (x :+ b))/2
--       h = (b - a) / fromInteger(n)
--       x = pi / 4
