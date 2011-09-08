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
      a0     = show $ a param
      b0     = show $ b param
      abstau = show $ BlastModel.Simple.tau param
      precision0 = show $ precision param
  in "Phi0 = " ++ phi0 ++ ", V0 = " ++ v0 ++ ", Alpha0 = " ++ alpha0 ++ ", A = " ++ a0 ++ ", B = " ++ b0 ++ ", tau = " ++ abstau ++ ", precision = " ++ precision0

--------------------------------------------------------------------------------

-- Default tau
tau' = 0.6

-- Default calc parameters
calc_params = CalcParams{
  -- Points are A, B, C and D
  points = (((pi/4) :+ (pi*tau'/4)), (0 :+ (pi*tau'/4)), (0 :+ 0), ((pi/4) :+ 0)),
  AFCalc.tau = tau',
  n_integral = 20
  }

-- Program entry point with tests
main = do
  let param = calc_params{AFCalc.dzdu = (BlastModel.Simple.dzdu null_parameters)}
  datalist <- calcPoints param
  let linetitle = extract_param_names null_parameters
  plotArea linetitle datalist
  pngArea ("../img/" ++ linetitle ++ ".png") linetitle datalist
  return ()




