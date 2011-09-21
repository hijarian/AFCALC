module Main where

-- Импортируем собственно саму модель
import BlastModel.Simple

-- Module for representing the results of calculations as charts
import BlastModel.AsChart

-- Модуль для оценки времени выполнения той или иной функции. Самописный, на основе более низкоуровневых модулей
import Time 

-- Точка входа программы. Программа делает последовательно ряд шагов и завершается.
main = do
-- 1. Печатаем приглашение и описание того, кто мы такие вообще.
  printGreeting
-- 2. Загружаем параметры тем или иным способом
  param <- getParameters
-- 3. Печатаем параметры, с которыми в итоге работаем.
  printParameters param
-- 5. Вычисляем список точек на границе воронки взрыва
-- 5.5. Выводим данные оценки времени выполнения
  pointlist <- calcPoints param
--  let linetitle = extract_param_names new_param
  let linetitle = extract_param_names param
-- 6. Передаём список точек функции черчения графика, которая чертит график
  plotFullGraph linetitle pointlist
-- 7. Пишем, что всё прошло успешно, так что завершаемся
  printGoodbye
-- Точка входа программы END
  
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
----------------------------------------

----------------------------------------
-- 5. Вычисляем список точек на границе воронки взрыва. BEGIN

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
-- 7. Пишем, что всё прошло успешно, так что завершаемся. BEGIN
printGoodbye :: IO ()

printGoodbye = do
  putStrLn "All done, good bye."
-- 7. Пишем, что всё прошло успешно, так что завершаемся. END
----------------------------------------


