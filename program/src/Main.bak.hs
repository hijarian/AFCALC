module Main where

data CalcParams = CalcParams {
  alpha :: Double,
  tau :: Double,
  phi_0 :: Double,
  v_0 :: Double,
  n_theta :: Integer,
  n_cn :: Integer,
  n_integrate :: Integer,
  rad_a :: Double,
  rad_b :: Double
  }

type PointAtSupplementaryField = Complex Double
type PointAtOriginField = Complex Double
type DzDu = (PointAtSupplementaryField -> PointAtOriginField)

type PointLine = [(Double, Double)]
type SetOfPointLines = [PointLine]

type ComplexValuesLine = [Complex Double]
type SetOfComplexValuesLines = [ComplexValuesLine]


render :: CalcParams -> IO()
render params = ModelRenderer.renderLines $ calcLines (Model.dzdu params) (Model.sides params)

calcLines :: DzDu -> SetOfComplexValuesLines -> SetOfPointLines
calcLines f lines = map (calcLine f) lines

calcLine :: DzDu -> ComplexValuesLine -> PointLine
calcLine f line = map (calcPoint f) line

calcPoint :: DzDu -> PointAtOriginField -> 
calcPoint f u = toPoint $ f u

toPoint :: Complex Double -> (Double, Double)
toPoint u = (realPart u, imagPart u)

