module Main where
-- Main module for the 'Interactive' plotter
-- Will mass-print charts for different values of parameters
-- Uses Blast model of Kotlyar

-- We will use Blast model for now. Can import other if needed
import BlastModel.Model_1975_Kotlyar

-- Main libraries
import AFCalc
-- We will output the results not only as lists of points, but as charts too
import AFCalc.AsChart

-- We use printf for pretty-printing the coordinates of points
import Text.Printf

-- Simple custom module for profiling
-- TODO: Replace with more smart widespread module
import Time

-- Entry point. Here we start interactive session
main = do
  -- 1. Print greeting message
  printGreeting
  -- 2. Load parameters --- default or user-provided (interactive dialog here)
  -- Here we should get ModelParams and CalcParams
--  (raw_mpar, raw_cpar) <- getParameters
  -- TODO: write implementation of getParameters which returns both ModelParams and CalcParams
  raw_mpar <- getParameters
  -- 3. Print loaded parameters
--  printParameters mpar cpar
  -- TODO: write implementation of printParameters which prints both ModelParams and CalcParams
  printParameters raw_mpar
  -- 4. Recalc the Cn parameter in ModelParams, model of Kotlyar relies on them
  mpar <- renewCn raw_mpar
  -- 5. Load calculation parameters
  cpar = loadModel mpar raw_cpar
  -- 5. Вычисляем список точек на границе воронки взрыва
  pointlist <- calcPoints mpar 
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

extract_param_names param =
  let phi0   = show $ phi_0 param
      v0     = show $ v_0 param
      alpha0 = show $ alpha param
      a0     = show $ a param
      b0     = show $ b param
      abstau = show $ tau param
      precision0 = show $ precision param
  in "Phi0 = " ++ phi0 ++ ", V0 = " ++ v0 ++ ", Alpha0 = " ++ alpha0 ++ ", A = " ++ a0 ++ ", B = " ++ b0 ++ ", tau = " ++ abstau ++ ", precision = " ++ precision0
-- 3. Печатаем параметры, с которыми в итоге работаем. END
----------------------------------------

----------------------------------------
-- 4. Renew Cn parameters needed for full model
-- As input accepting old model parameters
-- Returns renewed parameters containing changed Cn list
-- TODO: replace this with definition of two-argument function of ModelParams and Cn list
renewCn :: ModelParams -> IO (ModelParams)
renewCn param = do
  putStrLn "Second, we compute the parameters c_n needed for computations"
  -- We use Fourier method for computations
  new_param <- time $ renew_cn_fourier param
  return new_param

-- Renew Cn list, saved in mpar
-- Returns mpar with new Cn list
-- Based on Fourier method of simple iterations
renew_cn_fourier :: ModelParams -> IO (ModelParams)
renew_cn_fourier mpar = do
  print "renew_cn_all started"
  -- calc_new_cnlist defined in BlastModel.Model_1975_Kotlyar
  new_mpar <- calc_new_cnlist mpar
  err <- high_error 0.0001 mpar new_mpar
  if err == True
    then renew_cn_fourier new_param cpar
    else return $ new_param

-- 4. Обновляем в параметрах список коэффициентов cN ``c_n param``. END
----------------------------------------

----------------------------------------
-- 5. Вычисляем список точек на границе воронки взрыва. BEGIN
calcPoints :: ModelParams -> IO (ZPlanePoints)
calcPoints param = do
  print "We will now compute points on the edge of blast."
  let ad_pointlist =  (init.tail) $ zlistAD param
      ba_pointlist =  (init.tail) $ zlistBA param
      cb_pointlist =  (init.tail) $ zlistCB param
      dc_pointlist =  (init.tail) $ zlistDC param
  --5.5. TODO: Выводим данные оценки времени выполнения
  print "Points at A->D: "
  time $ outputData ad_pointlist
  print "Points at B->A: "
  time $ outputData ba_pointlist
  print "Points at D->C: "
  time $ outputData dc_pointlist
  print "Points at C->B: "
  time $ outputData cb_pointlist
  return (cb_pointlist, dc_pointlist, ad_pointlist, ba_pointlist)

outputData datalist = do
  mapM (\(x, y) ->  printf "%10.4f; %10.4f\n" x y) datalist
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

