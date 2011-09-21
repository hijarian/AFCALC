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



--------------------------------------------------------------------------------
-- OBSOLETE FUNCTIONS 2011.09.08
-- Making full list of Z points along lines DC, CB, BA, AD in that exact order
-- We need to preserve order because we calculate points by integration
-- zlist param = (
--   (zlistDC param),
--   (zlistCB param),
--   (zlistBA param),
--   (zlistAD param)
--   )
--
-- -- Coordinates of points of rectangular area of var U
-- -- TODO: it should be defined in model!
-- d' = (pi/4, 0)
-- c' = (0, 0)
-- b' param = (0, (pi/4)*(tau param))
-- a' param = (pi/4, (pi/4)*(tau param))
--
-- -- List of points on sides at Z corresponding to sides at U
--
-- zlistDC param = zlistHorizontal param dx cx cy
--   where (dx, _) = d'
--         (cx, cy) = c'
--
-- zlistCB param = zlistVertical   param cy by cx
--   where (cx, cy) = c'
--         (_, by) = b' param
--
-- zlistBA param = zlistHorizontal param bx ax ay
--   where (ax, ay) = a' param
--         (bx, _) = b' param
--
-- zlistAD param = zlistVertical    param ay dy ax
--   where (ax, ay) = a' param
--         (_, dy) = d'
--
--
-- -- Construct point at vertical line between (<x>, <y0>) and (<x>, <y>)
-- -- by integration and return tuple (<x>, <y>)
-- zVertical ::  ModelParams -> Double -> Double -> Double -> Complex Double
-- zVertical param x y0 y  = integrateVertical (dzdu param) n' y0 y x
--   where n' = n_integral param
--
-- -- Making points along vertical line between (<x>, <ya>) and (<x>, <yb>)
-- zlistVertical ::  ModelParams -> Double -> Double -> Double -> [(Double, Double)]
-- zlistVertical param ya yb x = map constructPoint $ quantize ya yb n'
--   where
--     constructPoint = asPoint . (zVertical param x ya)
--     n' = n_integral param
--
-- -- Construct point at horizontal line between (<x0>, <y>) and (<x>, <y>)
-- -- by integration and return tuple (<x>, <y>)
-- zHorizontal ::  ModelParams -> Double -> Double -> Double -> Complex Double
-- zHorizontal param y x0 x  = integrateVertical (dzdu param) n' x0 x y
--   where n' = n_integral param
--
-- -- Making points along horizontal line between (<xa>, <y>) and (<xb>, <y>)
-- zlistHorizontal ::  ModelParams -> Double -> Double -> Double -> [(Double, Double)]
-- zlistHorizontal param xa xb y = map constructPoint $ quantize xa xb n'
--   where
--     constructPoint = asPoint . (zHorizontal param y xa)
--     n' = n_integral param
--
--------------------------------------------------------------------------------
-- OBSOLETE FUNCTIONS
-- plotGraph :: String -> PointList -> IO()
-- plotGraph linetitle datalist = do
--     renderableToWindow  (toRenderable (chart linetitle datalist)) 640 480
--     renderableToPNGFile (toRenderable (chart linetitle datalist)) 640 480 ("img/" ++ linetitle ++ ".png")
--     return ()
--
-- plotFullGraph :: String -> ZPlanePoints -> IO()
-- plotFullGraph linetitle datalists = do
--   showFullGraph linetitle datalists
--   pngFullGraph linetitle datalists
--   return ()
--
-- showFullGraph linetitle datalists =
--     renderableToWindow  (toRenderable (manychart linetitle datalists)) 640 480
--
-- pngFullGraph linetitle datalists =
--     renderableToPNGFile (toRenderable (manychart linetitle datalists)) 640 480 ("img/" ++ linetitle ++ ".png")
--
--
--
--
-- manychart :: String -> ZPlanePoints -> Layout1 Double Double
-- manychart linetitle (pointsAD, pointsDC, pointsCB, pointsBA) = layout
--   where
--     plotAD = plot_lines_values ^= [pointsAD]
--               $ plot_lines_style .> line_color ^= opaque red
--               $ plot_lines_title ^= "A->D"
--               $ defaultPlotLines
--     plotDC = plot_lines_values ^= [pointsDC]
--               $ plot_lines_style .> line_color ^= opaque green
--               $ plot_lines_title ^= "D->C"
--               $ defaultPlotLines
--     plotCB = plot_lines_values ^= [pointsCB]
--               $ plot_lines_style .> line_color ^= opaque blue
--               $ plot_lines_title ^= "C->B: " ++ linetitle
--               $ defaultPlotLines
--     plotBA = plot_lines_values ^= [pointsBA]
--               $ plot_lines_style .> line_color ^= opaque silver
--               $ plot_lines_title ^= "B->A"
--               $ defaultPlotLines
--     layout = layout1_title ^= "Form of blast edge"
--            $ layout1_plots ^= [
--              Left (toPlot plotAD),
--              Left (toPlot plotDC),
--              Left (toPlot plotCB),
--              Left (toPlot plotBA)
--              ]
--            $ defaultLayout1

-- Пример оформления чертежа
-- -- chart = layout
-- --   where
-- --     am :: Double -> Double
-- --     am x = (sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))
-- --     sinusoid1 = plot_lines_values ^= [[ (x,(am x)) | x <- [0,(0.5)..400]]]
-- --               $ plot_lines_style  .> line_color ^= opaque blue
-- --               $ plot_lines_title ^= "am"
-- --               $ defaultPlotLines
-- --     sinusoid2 = plot_points_style ^= filledCircles 2 (opaque red)
-- --               $ plot_points_values ^= [ (x,(am x)) | x <- [0,7..400]]
-- --               $ plot_points_title ^= "am points"
-- --               $ defaultPlotPoints
-- --     layout = layout1_title ^= "Amplitude Modulation"
-- --            $ layout1_plots ^= [Left (toPlot sinusoid1),
-- --                                Left (toPlot sinusoid2)]
-- --            $ defaultLayout1

-- 6. Передаём список точек функции черчения графика, которая чертит график. END
----------------------------------------

{-

-- Make list of complex points with ordinate == y
-- (Points will effectively be at the line parallel to the 0x)
-- \xvalues - List of abscissa values
-- return value - list of complex numbers
makeXPoints :: [Double] -> Double -> [Complex Double]
makeXPoints xvalues y = map (:+ y) xvalues

-- Make list of complex points with abscissa == x
-- (Points will effectively be at the line parallel to the 0y)
-- \yvalues - List of ordinate values
-- return value - list of complex numbers
makeYPoints :: [Double] -> Double -> [Complex Double]
makeYPoints yvalues x = map ((:+) x) yvalues -- DO NOT REMOVE BRACKETS AROUND ':+'


-- Integrate complex function <f> along the straight line
--  between points (<x>, <ya>) and (<x>, <yb>)
-- <n> is a discretization coefficient
integrateVertical :: (Complex Double -> Complex Double) -> Integer -> Double -> Double -> Double -> Complex Double
integrateVertical f n ya yb x =
    (sum $ map f ypoints) * h
    where
      ypoints = makeYPoints yvalues x
      yvalues = quantize ya yb n
      h = getQuantizer (x :+ ya) (x :+ yb) n

-- Integrate complex function <f> along the straight line
--  between points (<xa>, <y>) and (<xb>, <y>)
-- <n> is a discretization coefficient
integrateHorizontal :: (Complex Double -> Complex Double) -> Integer -> Double -> Double -> Double -> Complex Double
integrateHorizontal f n xa xb y =
    (sum $ map f xpoints) * h
    where
      xpoints = makeYPoints xvalues y
      xvalues = quantize xa xb n
      h = getQuantizer (xa :+ y) (xb :+ y) n

integrateComplex :: (Complex Double -> Complex Double) -> Integer -> Complex Double -> Complex Double -> Complex Double
integrateComplex f n za zb =
  (sum $ map f zpoints) * h
    where
      zpoints = quantizeCenter za zb n
      h = getQuantizer za zb n

-- Integrate real function of one variable using method of trapezies
-- That same method is used in integrateHorizontal and integrateVertical
integrateReal :: (Double -> Double) -> Integer -> Double -> Double -> Double
integrateReal f n xa xb =
    ((sum $ map f xpoints) + t) * h
    where
      xpoints = quantize xa xb n
      t = ((f xa) + (f xb)) / 2
      h = getQuantizer xa xb n
-}

--
-- integrate :: (Complex Double -> Complex Double) -> Integer -> Complex Double -> Complex Double -> Complex Double
-- integrate = integrateWith CenterPoint
--
-- integrateWith :: QuantizeType -> (Complex Double -> Complex Double) -> Integer -> Complex Double -> Complex Double -> Complex Double
-- integrateWith t f n za zb =
--   (sum $ map f zpoints) * h
--     where
--       zpoints = quantize' t za zb n
--       h = getQuantizer za zb n

-- Function for testing some model with given alpha value and default tau value
-- Outputs to PNG so useful for silent mass tests
-- testAlpha :: ModelType -> Double -> IO()
-- testAlpha mtype a = do
--   let mpar = model_params{alpha = a}
--   let cpar = loadModel mtype mpar
--   processParams ToPNG mpar cpar
--   return ()

-- Function for testing some model with given tau value and default alpha value
-- Outputs to PNG so useful for silent mass tests
-- testTau :: ModelType -> Double -> IO()
-- testTau mtype t = do
--   let mpar  = model_params{tau = t}
--   let cpar = loadModel mtype mpar
--   processParams ToPNG mpar cpar
--   return ()
