module Main where

-- Импортируем собственно саму модель
import BlastModel.ExpTest
import AFCalc
import AFCalc.AsChart

-- Модуль для оценки времени выполнения той или иной функции. Самописный, на основе более низкоуровневых модулей
import Time

-- We use printf for pretty-printing the coordinates of points
import Text.Printf

----------------------------------------
-- 5. Вычисляем список точек на границе воронки взрыва. BEGIN

plusPoint (p1_x, p1_y) (p2_x, p2_y) = (p1_x + p2_x, p1_y + p2_y)

calcP1 param = (ad_pointlist, dc_pointlist, cb_pointlist, ba_pointlist)
  where
    ad_pointlist =  zlistAD param
    d_point = last ad_pointlist
    dc_pointlist =  map (plusPoint d_point) $ zlistDC param
    c_point = last dc_pointlist
    cb_pointlist =  map (plusPoint c_point) $ zlistCB param
    b_point = last cb_pointlist
    ba_pointlist =  map (plusPoint b_point) $ zlistBA param
    
calcP2 param = (ad_pointlist, dc_pointlist, cb_pointlist, ba_pointlist)
  where
    ad_pointlist =  zlistAD param
    dc_pointlist =  zlistDC param
    cb_pointlist =  zlistCB param
    ba_pointlist =  zlistBA param

calcPoints :: ModelParams -> IO (ZPlanePoints)
calcPoints param = do
  print "We will now compute points on the edge of blast."
  let (ad_pointlist, dc_pointlist, cb_pointlist, ba_pointlist) = calcP1 param
  --5.5. TODO: Выводим данные оценки времени выполнения
  print "Points at A->D: "
  time $ outputData ad_pointlist
  print "Points at D->C: "
  time $ outputData dc_pointlist
  print "Points at C->B: "
  time $ outputData cb_pointlist
  print "Points at B->A: "
  time $ outputData ba_pointlist
  return (ad_pointlist, dc_pointlist, cb_pointlist, ba_pointlist)

outputData datalist = do
  mapM (\(x, y) ->  printf "%10.4f; %10.4f\n" x y) datalist
  return ()


-- 5. Вычисляем список точек на границе воронки взрыва. END
----------------------------------------

-- Program entry point with tests
-- should make this runnable from program argument
main = do
  silentlyPlotGraph null_parameters

-- Автоматизированные тесты для некоторого покрытия поля параметров модели
silentlyPlotGraph :: ModelParams -> IO()
silentlyPlotGraph param = do
  pointlist <- calcPoints param
  let linetitle = "tau = " ++ (show (tau param))
  pngFullGraph linetitle pointlist
  return ()

