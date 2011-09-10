module Main where

-- Importing the model
import BlastModel.Model_1975_Kotlyar

-- Importing the calculator
import AFCalc
import AFCalc.AsChart

-- Importing the pretty-printer for floats
import Text.Printf

-- Custom module for profiling, maybe I will use something more robust
import Time

-- We ARE using complex numbers
import Data.Complex

-- Default tau
tau' = 0.4

-- Default model parameters
model_params = null_parameters{
    BlastModel.Simple.tau = tau',
    alpha = 1.3
  }

-- Default calc parameters
calc_params = CalcParams{
  -- Points are A, B, C and D
  points = (((pi/4) :+ (pi*tau'/4)), (0 :+ (pi*tau'/4)), (0 :+ 0), ((pi/4) :+ 0)),
  AFCalc.tau = tau', -- value of tau, do not use here values > 1
  n_integral = 20, -- number of integrations
  origin = ((pi/4) :+ (pi*tau'/4)), -- base point for integration
  folder = "img/", -- folder to hold generated images
-- Fillers for initialization
  AFCalc.dzdu = id,
  AFCalc.dwdu = id,
  AFCalc.chi_0 = id,
  AFCalc.f_corr = id
  }

-- Program entry point with tests
main = do
  mapM (testTau.(* 0.1)) [1..10]
  mapM (testAlpha.(* 0.1)) [13..20]
  return ()

testAlpha a = do
  let mpar = model_params{alpha = a}
  let cpar = calc_params{AFCalc.dzdu = (dzdu' mpar)}
  processParams ToPNG mpar cpar
  return ()

testTau t = do
  let mpar = model_params{BlastModel.Simple.tau = t}
  let cpar = calc_params{AFCalc.tau = t, AFCalc.dzdu = (dzdu' mpar)}
  processParams ToPNG mpar cpar
  return ()

-- Probing model with given tau and alpha parameters
-- ATTENTION: we need to synchronize the tau values in AFCALC and our model!
testAlphaTauWith :: RenderType -> Double -> Double -> IO()
testAlphaTauWith r a t = do
  let mpar = model_params{BlastModel.Simple.tau = t, alpha = a}
  let cpar = calc_params{AFCalc.tau = t, AFCalc.dzdu = (dzdu' mpar)}
  processParams r mpar cpar
  return ()

-- for use with ghci
testAlphaTau = testAlphaTauWith ToWindow

--------------------------------------------------------------------------------
-- Main function to get data from model
-- Will consume CalcParams object containing dwdu, chi_0 and f_corr functions
-- If model uses ModelParams objects, then CalcParams.dwdu, for example, should contain
--   (dwdu mpar) curried function
calcPoints :: CalcParams -> IO (ZPlanePoints)
calcPoints param = do
  print "We will now compute points on the edge of blast."
-- We will not calculate to (d' param), we stop at 99%
  let dx = realPart (d' param)
  let dy = imagPart (d' param)
  let d1' = ((dx + 0.01) :+ dy)
  let d2' = (dx :+ (dy + 0.01))
  -- Getting every side of area
  let (ab, bc, cd, da) = (
	calcSide param (a' param) (b' param),
	calcSide param (b' param) (c' param),
	calcSide param{origin=( (pi/4 - 0) :+ (pi*tau'/4 + 0)) } (c' param) d1',
	calcSide param{origin=( (pi/4 + 0) :+ (pi*tau'/4 + 0)) } d2' (a' param))
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

-- Print pointlist each point on separate line coordinates delimited by ';'
outputData datalist = do
  mapM (\(x, y) ->  printf "%10.4f; %10.4f\n" x y) datalist
  return ()

-- Converts model parameters to string
extract_param_names param =
  let phi0   = phi_0 param
      v0     = v_0 param
      alpha0 = alpha param
      abstau = BlastModel.Simple.tau param
  in printf "Phi0 = %4.2f, v0 = %4.2f, Alpha = %2.1f, tau = %2.1f" phi0 v0 alpha0 abstau

-- Type of rendering target
data RenderType = ToWindow | ToPNG

--function to plot single chart using given ModelParams and CalcParams
processParams :: RenderType -> ModelParams -> CalcParams -> IO()
processParams rtype mpar cpar = do
  datalist <- calcPoints cpar
  let linetitle = extract_param_names mpar
  showChart rtype cpar linetitle datalist
  return ()

--function to plot single chart using given ModelParams and CalcParams
showChart :: RenderType -> CalcParams -> String -> PointList -> IO()
-- Showing chart to GTK window
showChart ToWindow _ linetitle datalist = do
  plotArea linetitle datalist
  return ()

-- Showing chart to PNG file 
showChart ToPNG cpar linetitle datalist = do
  let filename = (folder cpar) ++ linetitle ++ ".png"
  pngArea filename linetitle datalist
  return ()

