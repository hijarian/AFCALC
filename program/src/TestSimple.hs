module Main where

-- Importing the model
import BlastModel.Simple

-- Importing the calculator
import AFCalc
import AFCalc.AsChart

-- Importing the pretty-printer for floats
import Text.Printf

-- Custom module for profiling, maybe I will use something more robust
import Time

-- We ARE using complex numbers
import Data.Complex

--------------------------------------------------------------------------------
calcPoints :: CalcParams -> IO (ZPlanePoints)
calcPoints param = do
  print "We will now compute points on the edge of blast."
  -- Getting every side of area
  let (ab, bc, cd, da) = calcArea param
  -- Logging
  print "Points at AB: "
  time $ outputData ab
  print "Points at BC: "
  time $ outputData bc
  print "Points at CD: "
  time $ outputData cd
  print "Points at DA: "
  time $ outputData da
  return (ab, bc, cd, da)

outputData datalist = do
  mapM (\(x, y) ->  printf "%10.4f; %10.4f\n" x y) datalist
  return ()

extract_param_names param =
  let phi0   = show $ phi_0 param
      v0     = show $ v_0 param
      alpha0 = show $ alpha param
      abstau = show $ BlastModel.Simple.tau param
  in printf "Phi0 = %4.2f, v0 = %4.2f, Alpha = %2.1f, tau = %2.1f" phi0 v0 alpha0 abstau

--------------------------------------------------------------------------------

-- Default tau
tau' = 0.6

-- Default model parameters
model_params = null_parameters{
    BlastModel.Simple.tau = tau',
    alpha = 0.7
  }

-- Default calc parameters
calc_params = CalcParams{
  -- Points are A, B, C and D
  points = (((pi/4)*(0.99) :+ (pi*tau'/4)), (0 :+ (pi*tau'/4)), (0 :+ 0), ((pi/4)*(0.99) :+ 0)),
  AFCalc.tau = tau',
  n_integral = 20
  }

-- Program entry point with tests
main = do
  mapM (testTau.(* 0.1)) [1..10]
  mapM (testAlpha.(* 0.1)) [13..20]
  return ()

testAlpha a = do
  let mpar = model_params{alpha = a}
  let cpar = calc_params{AFCalc.dzdu = (BlastModel.Simple.dzdu mpar)}
  datalist <- calcPoints cpar
  let linetitle = extract_param_names mpar
--  plotArea linetitle datalist
  pngArea ("img/" ++ linetitle ++ ".png") linetitle datalist
  return ()

testTau t = do
  let mpar = model_params{BlastModel.Simple.tau = t}
  let cpar = calc_params{AFCalc.tau = t, AFCalc.dzdu = (BlastModel.Simple.dzdu mpar)}
  datalist <- calcPoints cpar
  let linetitle = extract_param_names mpar
--  plotArea linetitle datalist
  pngArea ("img/" ++ linetitle ++ ".png") linetitle datalist
  return ()

-- for use with ghci
testAlphaTau a t = do
  let mpar = model_params{BlastModel.Simple.tau = t, alpha = a}
  let cpar = calc_params{AFCalc.tau = t, AFCalc.dzdu = (BlastModel.Simple.dzdu mpar)}
  datalist <- calcPoints cpar
  let linetitle = extract_param_names mpar
  plotArea linetitle datalist
  pngArea ("img/" ++ linetitle ++ ".png") linetitle datalist
  return ()

