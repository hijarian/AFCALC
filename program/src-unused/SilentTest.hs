module Main where
-- Main module for special tester program using AFCALC to mass-generate charts with varying parameters of model

-- Model module
import BlastModel.Full

-- Core modules for processing and output
import AFCalc
import AFCalc.AsChart

main = do
  testAlpha null_parameters{a=1}
  testTau   null_parameters{alpha=0.7,a=1}
  return ()
  
-- Автоматизированные тесты для некоторого покрытия поля параметров модели
silentlyPlotGraph :: ModelParams -> IO()
silentlyPlotGraph param = do
  new_param <- renewCoeffs param
  pointlist <- calcPoints new_param
  let linetitle = extract_param_names new_param
  pngFullGraph linetitle pointlist
  return ()

-- Autotesting Phi_0 parameter 20 times from 0 to 2 step 0.1
testPhi0 :: ModelParams -> IO()
testPhi0 param = do 
  let plotWithPhi0 p = silentlyPlotGraph param{phi_0 = (p * 0.1)}
  mapM plotWithPhi0 [0..20]
  return ()

-- Autotesting V_0 parameter 10 times from 0 to 150 step 15
testV0 :: ModelParams -> IO()
testV0 param = do 
  let plotWithV0 p = silentlyPlotGraph param{v_0 = (p * 15)}
  mapM plotWithV0 [0..10]
  return ()

-- Autotesting Tau parameter 10 times from 0 to 1 step 0.1
testTau :: ModelParams -> IO()
testTau param = do 
  let plotWithTau p = silentlyPlotGraph param{tau = (p * 0.1)}
  mapM plotWithTau [1..10]
  return ()

-- Autotesting Alpha parameter 20 times from 0 to 1 step 0.05
testAlpha :: ModelParams -> IO()
testAlpha param = do 
  let plotWithAlpha p = silentlyPlotGraph param{alpha = (p * 0.05)}
  mapM plotWithAlpha [0..1]
  return ()

