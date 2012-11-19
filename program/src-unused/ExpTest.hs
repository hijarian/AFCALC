module BlastModel.ExpTest where
-- Module defining dzdu as simple exp function
-- We will use it as test suite for integrators and point generators

-- We work with complex numbers
import Data.Complex

data ModelParams = ModelParams {
    tau        :: Double,     -- параметр, доопределяет тета-функции
    n_integral :: Integer,    -- частота разбиения отрезка интегрирования
    precision  :: Double      -- точность вычисления cN методом простых итераций. Да и вообще "точность" там, где она может быть нужна
    } deriving (Show)

null_parameters = ModelParams {
  tau        = 0.7,
  n_integral = 50,
  precision  = 0.001
  }

dzdu :: (RealFloat a) => ModelParams -> Complex a -> Complex a
dzdu _ u = exp u


