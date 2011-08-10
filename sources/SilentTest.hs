module Main where

-- Скорее всего, здесь не понадобится этот модуль, потому что работа с комплексными числами будет инкапсулирована в модуле BlastModel
--import Data.Complex

-- Импортируем собственно саму модель
import BlastModel

-- Модули, необходимые для поддержки черчения графиков функций
import Graphics.Rendering.Chart 
import Graphics.Rendering.Chart.Gtk
import Data.Colour
import Data.Colour.Names
import Data.Accessor

-- Модуль для оценки времени выполнения той или иной функции. Самописный, на основе более низкоуровневых модулей
import Time 

-- Точка входа программы. Программа делает последовательно ряд шагов и завершается.
-- main = do
-- -- 1. Печатаем приглашение и описание того, кто мы такие вообще.
--   printGreeting
-- -- 2. Загружаем параметры тем или иным способом
--   param <- getParameters
-- -- 3. Печатаем параметры, с которыми в итоге работаем.
--   printParameters param
-- -- 4. Обновляем в параметрах список коэффициентов cN ``c_n param``
-- -- 4.5. Выводим данные оценки времени выполнения
--   new_param <- renewCoeffs param
-- -- 5. Вычисляем список точек на границе воронки взрыва
-- -- 5.5. Выводим данные оценки времени выполнения
--   pointlist <- calcPoints new_param
--   let linetitle = extract_param_names new_param
-- -- 6. Передаём список точек функции черчения графика, которая чертит график
--   plotFullGraph linetitle pointlist
-- -- 7. Пишем, что всё прошло успешно, так что завершаемся
--   printGoodbye
-- -- Точка входа программы END
  
----------------------------------------
-- 1. Печатаем приглашение и описание того, кто мы такие вообще. BEGIN
printGreeting :: IO ()
printGreeting = do  
  putStrLn "Greetings!~ This is blast model, based on solid-liquid model of Lavrentyev and Kotlyar."  
-- 1. Печатаем приглашение и описание того, кто мы такие вообще. END
----------------------------------------

----------------------------------------
-- 2. Загружаем параметры тем или иным способом. BEGIN

getParameters :: IO (ModelParams)
getParameters = do
  print "[1] --- load parameters manually"
  print "[anykey] --- load defaults"
  print "Parameters n_integral, n_cn, n_theta stay constant"
  what <- getLine
  if what == "1"
    then do print "Ok, will load parameters from you" 
            getModelParameters
    else do print "Ok, will load default parameters"
            return null_parameters

getModelParameter :: String -> IO(Double)
getModelParameter parname = do
	putStr $ "Value of " ++ parname
	param <- getLine
        return $ read param
  
getModelParameters :: IO (ModelParams)
getModelParameters = do
  print      "Value of phi_0:"
  phi_0'       <- getLine
  print      "Value of v_0:"
  v_0'         <- getLine
  print      "Value of |tau|:"
  tau'         <- getLine
  print      "Value of alpha:"
  alpha'       <- getLine
  print      "Value of a:"
  a'           <- getLine
  print      "Value of b:"
  b'           <- getLine
  print      "Precision:"
  precision'   <- getLine
  -- Параметры точности константные, потому что.
  -- n_theta'     <- getModelParameter "n_theta"
  -- n_integral'  <- getModelParameter "n_integral"
  -- n_cn'        <- getModelParameter "n_cn"
  print       "Filler for c_n:"
  fill_cn      <- getLine
  return ModelParams{
       phi_0      = read phi_0',
       v_0        = read v_0',
       tau        = read tau',
       alpha      = read alpha',
       a          = read a',
       b          = read b',
       precision  = read precision',
       -- Берём параметры точности из параметров по умолчанию
       n_theta    = n_theta    null_parameters,
       n_integral = n_integral null_parameters,
       n_cn       = n_cn       null_parameters,
       -- Начальные значения коэффициентов приравниваем к значению-заполнителю
       c_n        = replicate ((fromInteger . n_cn) null_parameters) (read fill_cn)
       }
    
-- 2. Загружаем параметры тем или иным способом. END
----------------------------------------

----------------------------------------
-- 3. Печатаем параметры, с которыми в итоге работаем. BEGIN
printParameters :: ModelParams -> IO ()
printParameters param = do
-- TODO: Напиши меня!
  print "Current model parameters: "
  print param
-- 3. Печатаем параметры, с которыми в итоге работаем. END
----------------------------------------

----------------------------------------
-- 4. Обновляем в параметрах список коэффициентов cN ``c_n param``. BEGIN
renewCoeffs :: ModelParams -> IO (ModelParams)
renewCoeffs param = do
  putStrLn "Second, we compute the parameters c_n needed for computations"
-- 4.5. Выводим данные оценки времени выполнения
  new_param <- time $ renew_cn_all param
  return new_param

-- 4. Обновляем в параметрах список коэффициентов cN ``c_n param``. END
----------------------------------------

----------------------------------------
-- 5. Вычисляем список точек на границе воронки взрыва. BEGIN
type PointList = [(Double, Double)]
type BAPointList = PointList
type CDPointList = PointList
type CBPointList = PointList
type DAPointList = PointList
type ZPlanePoints = (CBPointList, CDPointList, DAPointList, BAPointList)
calcPoints :: ModelParams -> IO (ZPlanePoints)
calcPoints param = do
  print "We will now compute points on the edge of blast."
  let da_pointlist = (init.tail) $ zlistDA param 
      ba_pointlist = (init.tail) $ zlistBA param 
      cb_pointlist = (init.tail) $ zlistCB param 
      cd_pointlist = (init.tail) $ zlistCD param 
  --5.5. TODO: Выводим данные оценки времени выполнения
  print "Points at DA: "
  time $ outputData da_pointlist
  print "Points at BA: "
  time $ outputData ba_pointlist
  print "Points at CD: "
  time $ outputData cd_pointlist
  print "Points at CB: "
  time $ outputData cb_pointlist
  return (cb_pointlist, cd_pointlist, da_pointlist, ba_pointlist)
  
outputData datalist = do 
  mapM (\(x, y) ->  putStrLn $ (show x) ++ "; " ++ (show y)) datalist
  return ()

-- 5. Вычисляем список точек на границе воронки взрыва. END
----------------------------------------

----------------------------------------
-- 6. Передаём список точек функции черчения графика, которая чертит график. BEGIN

extract_param_names param = 
  let phi0   = show $ phi_0 param
      v0     = show $ v_0 param
      alpha0 = show $ alpha param
      a0     = show $ a param
      b0     = show $ b param
      abstau = show $ tau param
      precision0 = show $ precision param
  in "Phi0 = " ++ phi0 ++ ", V0 = " ++ v0 ++ ", Alpha0 = " ++ alpha0 ++ ", A = " ++ a0 ++ ", B = " ++ b0 ++ ", tau = " ++ abstau ++ ", precision = " ++ precision0

plotGraph :: String -> PointList -> IO()
plotGraph linetitle datalist = do
    renderableToWindow  (toRenderable (chart linetitle datalist)) 640 480
    renderableToPNGFile (toRenderable (chart linetitle datalist)) 640 480 (linetitle ++ ".png")
    return ()

plotFullGraph :: String -> ZPlanePoints -> IO()
plotFullGraph linetitle datalists = do
    renderableToWindow  (toRenderable (manychart linetitle datalists)) 640 480
    renderableToPNGFile (toRenderable (manychart linetitle datalists)) 640 480 (linetitle ++ ".png")
    return ()

chart :: String -> PointList -> Layout1 Double Double 
chart linetitle datalist = layout
  where
    myPlot = plot_lines_values ^= [datalist]
              $ plot_lines_style .> line_color ^= opaque blue
              $ plot_lines_title ^= linetitle
              $ defaultPlotLines
    layout = layout1_title ^= "Form of blast edge"
           $ layout1_plots ^= [Left (toPlot myPlot)]
           $ defaultLayout1

manychart :: String -> ZPlanePoints -> Layout1 Double Double 
manychart linetitle (pointsCB, pointsCD, pointsBA, pointsDA) = layout
  where
    -- plotBA = plot_lines_values ^= [pointsBA]
    --           $ plot_lines_style .> line_color ^= opaque green 
    --           $ plot_lines_title ^= "BA" 
    --           $ defaultPlotLines
    -- plotDA = plot_lines_values ^= [pointsDA]
    --           $ plot_lines_style .> line_color ^= opaque red
    --           $ plot_lines_title ^= "DA" 
    --           $ defaultPlotLines
    plotCB = plot_lines_values ^= [pointsCB]
              $ plot_lines_style .> line_color ^= opaque blue
              $ plot_lines_title ^= "CB: " ++ linetitle
              $ defaultPlotLines
    -- plotCD = plot_lines_values ^= [pointsCD]
    --           $ plot_lines_style .> line_color ^= opaque cyan 
    --           $ plot_lines_title ^= "CD"
    --           $ defaultPlotLines
    layout = layout1_title ^= "Form of blast edge"
           $ layout1_plots ^= [
--             Left (toPlot plotBA), 
--             Left (toPlot plotDA), 
             -- Left (toPlot plotCD), 
             Left (toPlot plotCB)
             ]
           $ defaultLayout1

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

----------------------------------------
-- 7. Пишем, что всё прошло успешно, так что завершаемся. BEGIN
printGoodbye :: IO ()

printGoodbye = do
  putStrLn "All done, good bye."
-- 7. Пишем, что всё прошло успешно, так что завершаемся. END
----------------------------------------


main = do
--  testAlpha null_parameters{a=1}
  testTau null_parameters{alpha=0.7,a=1}
--  testA null_parameters
  silentlyPlotGraph null_parameters{alpha=0.49,tau=0.7,a=1}
  return ()
  
-- Автоматизированные тесты для некоторого покрытия поля параметров модели
silentlyPlotGraph :: ModelParams -> IO()
silentlyPlotGraph param = do
  new_param <- renewCoeffs param
  pointlist <- calcPoints new_param
  let linetitle = extract_param_names new_param
  renderableToPNGFile (toRenderable (manychart linetitle pointlist)) 640 480 (linetitle ++ ".png")
  return ()

testPhi0 :: ModelParams -> IO()
testPhi0 param = do 
  let plotWithPhi0 p = silentlyPlotGraph param{phi_0 = (p * 0.1)}
  mapM plotWithPhi0 [0..20]
  return ()

testV0 :: ModelParams -> IO()
testV0 param = do 
  let plotWithV0 p = silentlyPlotGraph param{v_0 = (p * 15)}
  mapM plotWithV0 [0..10]
  return ()

testTau :: ModelParams -> IO()
testTau param = do 
  let plotWithTau p = silentlyPlotGraph param{tau = (p * 0.1)}
  mapM plotWithTau [1..10]
  return ()

testAlpha :: ModelParams -> IO()
testAlpha param = do 
  let plotWithAlpha p = silentlyPlotGraph param{alpha = (p * 0.01)}
  mapM plotWithAlpha [65..75]
  return ()

testA :: ModelParams -> IO()
testA param = do 
  let plotWithA p = silentlyPlotGraph param{a = (p * 1),b=((p * 1) + 1)}
  mapM plotWithA [1..11]
  return ()
