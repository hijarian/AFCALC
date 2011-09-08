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
  in "Phi0 = " ++ phi0 ++ ", V0 = " ++ v0 ++ ", Alpha0 = " ++ alpha0 ++ ", tau = " ++ abstau

--------------------------------------------------------------------------------

-- Default tau
tau' = 0.6

-- Default model parameters
model_params = null_parameters{
    BlastModel.Simple.tau = tau',
    alpha = 0.3
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
  let param = calc_params{AFCalc.dzdu = (BlastModel.Simple.dzdu model_params)}
  datalist <- calcPoints param
  let linetitle = extract_param_names model_params
  plotArea linetitle datalist
  pngArea ("../img/" ++ linetitle ++ ".png") linetitle datalist
  return ()




