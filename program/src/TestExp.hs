module Main where

-- Импортируем собственно саму модель
import BlastModel.Exp
import AFCalc
import AFCalc.AsChart

-- Модуль для оценки времени выполнения той или иной функции. Самописный, на основе более низкоуровневых модулей
import Time

----------------------------------------
-- 5. Вычисляем список точек на границе воронки взрыва. BEGIN
-- WARNING: COPIED FROM Simple.hs and Main.hs

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

-- Program entry point with tests
-- should make this runnable from program argument
main = do
  testTau null_parameters
  testAlpha null_parameters

-- Автоматизированные тесты для некоторого покрытия поля параметров модели
silentlyPlotGraph :: ModelParams -> IO()
silentlyPlotGraph param = do
  pointlist <- calcPoints param
  let linetitle = extract_param_names param
  pngFullGraph linetitle pointlist
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
  let plotWithAlpha p = silentlyPlotGraph param{alpha = (p * 0.1)}
  mapM plotWithAlpha [0..10]
  return ()

