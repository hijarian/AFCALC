module Main where
-- Main module for the 'Tests' plotter
-- Will mass-print charts for different values of parameters
-- Uses Blast model of Kotlyar

-- hijarian @ 2011-09-21 14:15
-- Some thoughts about 'Tests' plotter.
-- I think that most useful test will be to generate chart for every combination
--   of tau and alpha parameters, then for every combination of rad_a and rad_b
--   parameters. So, there will be some sort of _grid_ of charts showing
--   mutations of area based on input parameters

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
tau' = tau model_params

-- Default model parameters
-- TODO: Maybe toss model_defaults from Model module completely?
model_params = model_defaults{
    tau   = 0.4, -- do not use here values > 1
    alpha = 1.3,
    rad_a = 2,
    rad_b = 2
  }

-- Default calc parameters
calc_params = CalcParams{
    -- Points are A, B, C and D
    -- TODO: points should be defined in the MODEL and simply passed by this
    --   module from model parameters to calculation parameters
    --   We define points there because the point D is a special point (infty)
    --   and we should avoid calculations of integral in it.
    --   We need some additional parameter called spec_points or so
    points     = (((pi/4) :+ (pi*tau'/4)), (0 :+ (pi*tau'/4)), (0 :+ 0), ((pi/4) :+ 0)),
    -- value of tau, do not use here values > 1
--     _tau = tau', 
    -- number of integrations
    n_integral = 20, 
    -- base point for integration
    origin     = ((pi/4) :+ (pi*tau'/4)), 
    -- folder to hold generated images
    folder     = "img/", 
    -- Fillers for initialization
    _dzdu      = id,
    _dwdu      = id,
    _chi_0     = id,
    _f_corr    = id,
    -- We can use user-provided simple forms of dzdu, but not by default
    is_simple  = False
}

-- Program entry point with tests
-- NOTE: I will play with this function until there is some clarity 
main = do
  mapM ((probeParam Tau   Simple) .(* 0.1)) [1..10]
  mapM ((probeParam Alpha Simple) .(* 0.1)) [1..20]
  mapM ((probeParam Tau   Full)  . (* 0.1)) [1..10]
  mapM ((probeParam Alpha Full)  . (* 0.1)) [1..20]
  return ()


--------------------------------------------------------------------------------
-- Type of parameter to test
data ParamType = Tau | Alpha | RadA | RadB | Phi0 | V0

-- Type of model used.
-- Full model consists of three functions: dwdu, chi_0 and f_corr
-- Simple model consists of two functions: dwdu and chi_0 (f_corr = 0)
-- Short model consists of only one function dzdu, containing full model
data ModelType = Short | Simple | Full deriving (Eq)


-- Type of rendering target
data RenderType = ToWindow | ToPNG

-- Will load model of given type based on given parameters
-- Returns CalcParams object for calculations
loadModel :: ModelType -> ModelParams -> CalcParams
-- Will create CalcParams object for calculations of full model
loadModel Full mpar = calc_params{
    folder = "img/full/",
    _dwdu   = (dwdu   mpar), 
    _chi_0  = (chi_0  mpar),
    _f_corr = (f_corr mpar)
    }

-- Will create CalcParams object for calculations of simple model without f_corr
loadModel Simple mpar = calc_params{
  folder = "img/simple/",
  _dwdu   = (dwdu   mpar), 
  _chi_0  = (chi_0  mpar),
  _f_corr = (\u -> (0 :+ 0))
  }

-- Will create CalcParams object for calculations of model defined as
--   singular function dzdu
loadModel Short mpar = calc_params{
  is_simple = True,
  folder = "img/short/",
  _dzdu   = (dzdu mpar) 
  }

-- Function for updating the model parameters.
-- If we use Full model, then we should update the Cn coefficients 
--   every time we change any of parameters
updateParams :: ModelType -> ParamType -> ModelParams -> Double -> IO(ModelParams)
updateParams t  
    | t == Full = updateParamsWithRecalc 
    | otherwise = setnewParams 

updateParamsWithRecalc :: ParamType -> ModelParams -> Double -> IO(ModelParams)
updateParamsWithRecalc p mpar v = do
    raw_new_mpar <- setnewParams p mpar v
    new_mpar     <- renewCn raw_new_mpar
    return new_mpar
    
-- Setting new parameters based on given ParamType
-- TODO: remove china code below and replace it with some cool type dispathing
setnewParams :: ParamType -> ModelParams -> Double -> IO(ModelParams)
setnewParams Tau   mpar v = return mpar{tau   = v}
setnewParams Alpha mpar v = return mpar{alpha = v}
setnewParams Phi0  mpar v = return mpar{phi_0 = v}
setnewParams V0    mpar v = return mpar{v_0   = v}
setnewParams RadA  mpar v = return mpar{rad_a = v}
setnewParams RadB  mpar v = return mpar{rad_b = v}

-- Simple probing function
-- renders to PNG so useful for silent tests
probeParam :: ParamType -> ModelType -> Double -> IO()
probeParam ptype mtype value = do
    mpar <- updateParams mtype ptype model_params value
    let cpar = loadModel mtype mpar
    processParams ToPNG mpar cpar
    return ()
    
-- Main testing function
-- Probing model with given tau and alpha parameters
-- Accepts RenderType and ModelType parameters
testAlphaTauWith :: RenderType -> ModelType -> Double -> Double -> IO()
testAlphaTauWith r m a t = do
  let raw_mpar = model_params{alpha = a}
  mpar <- updateParams m Tau raw_mpar t
  let cpar = loadModel m mpar
  processParams r mpar cpar
  return ()

-- shorthand for use with ghci
testAT = testAlphaTauWith ToWindow


--------------------------------------------------------------------------------
-- 4. Renew Cn parameters needed for full model
-- As input accepting old model parameters
-- Returns renewed parameters containing changed Cn list
-- TODO: replace this with definition of two-argument function 
--   of ModelParams and Cn list
renewCn :: ModelParams -> IO (ModelParams)
renewCn mpar = do
  putStrLn "Second, we compute the parameters c_n needed for computations"
  -- We use Fourier method for computations
  new_param <- time $ renew_cn_fourier mpar
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
    then renew_cn_fourier new_mpar
    else return $ new_mpar

-- 4. Renew Cn parameters needed for full model. END
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Main function to get data from model
-- Will consume CalcParams object containing dwdu, chi_0 and f_corr functions
-- If model uses ModelParams objects, then CalcParams.dwdu, for example, 
--   should contain (dwdu mpar) curried function
calcPoints :: CalcParams -> IO (ZPlanePoints)
calcPoints cpar = do
  print "We will now compute points on the edge of blast."
-- We will not calculate to (d' cpar), we stop at 99%
  let dx = realPart (d' cpar)
  let dy = imagPart (d' cpar)
  let d1' = ((dx + 0.01) :+ dy)
  let d2' = (dx :+ (dy + 0.01))
  -- Getting every side of area
  let (ab, bc, cd, da) = (
	calcSide cpar (a' cpar) (b' cpar),
	calcSide cpar (b' cpar) (c' cpar),
	calcSide cpar{origin=( (pi/4 - 0) :+ (pi*tau'/4 + 0)) } (c' cpar) d1',
	calcSide cpar{origin=( (pi/4 + 0) :+ (pi*tau'/4 + 0)) } d2' (a' cpar))
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
extract_param_names :: ModelParams -> String
extract_param_names mpar =
  let     
    print' = printf "Phi0 = %4.2f, V0 = %4.2f, Alpha = %2.1f, |Tau| = %2.1f"
    phi0   = phi_0 mpar
    v0     = v_0 mpar
    alpha0 = alpha mpar
    abstau = tau mpar
  in print' phi0 v0 alpha0 abstau

--function to plot single chart using given ModelParams and CalcParams
processParams :: RenderType -> ModelParams -> CalcParams -> IO()
processParams rtype mpar cpar = do
  datalist <- calcPoints cpar
  let linetitle = extract_param_names mpar
  showChart rtype cpar linetitle datalist
  return ()

--function to plot single chart using given ModelParams and CalcParams
--showChart :: RenderType -> CalcParams -> String -> ZPlanePoints -> IO()
-- Showing chart to GTK window
showChart ToWindow _ linetitle datalist = do
  plotArea linetitle datalist
  return ()

-- Showing chart to PNG file 
showChart ToPNG cpar linetitle datalist = do
  let filename = (folder cpar) ++ linetitle ++ ".png"
  pngArea filename linetitle datalist
  return ()

