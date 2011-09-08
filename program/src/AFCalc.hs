module AFCalc where
-- Module for getting actual points using functions defined in model


-- We use complex numbers
import Data.Complex


-- We use custom integrators written specifically for needs of AFCALC
import AFCalc.Integrators

--------------------------------------------------------------------------------
-- type declarations
type PointList = [(Double, Double)]
type ABPointList = PointList
type BCPointList = PointList
type CDPointList = PointList
type DAPointList = PointList
type ZPlanePoints = (ABPointList, BCPointList, CDPointList, DAPointList)

--------------------------------------------------------------------------------
-- It is a calculation parameters, holding every component needed to calculate points
-- It should be filled before calling any other method
data CalcParams = CalcParams {
    dwdu       :: (Complex Double -> Complex Double),
    chi_0      :: (Complex Double -> Complex Double),
    f_corr     :: (Complex Double -> Complex Double),
    points     :: (Complex Double, Complex Double, Complex Double, Complex Double), -- Points at A, B, C and D, respectively
    tau        :: Double,
    n_integral :: Integer
}

--------------------------------------------------------------------------------
-- Points defined in CalcParams
a' :: CalcParams -> Complex Double
a' params = p
    where (p, _, _, _) = points params

b' :: CalcParams -> Complex Double
b' params = p
    where (_, p, _, _) = points params

c' :: CalcParams -> Complex Double
c' params = p
    where (_, _, p, _) = points params

d' :: CalcParams -> Complex Double
d' params = p
    where (_, _, _, p) = points params
--------------------------------------------------------------------------
-- interface of AFCalc

-- Calculate only one point of target area
calcPoint :: CalcParams -> Complex Double -> Complex Double
calcPoint params u = integrateComplex (dzdu params) n' (a' params) u
    where n' = n_integral params

-- Calculate the line in target area corresponding to the line between
--  given points at source area
calcSide :: CalcParams -> Complex Double -> Complex Double -> PointList
calcSide params u1 u2 = map constructPoint $ quantizeCenter u1 u2 n'
    where
      constructPoint = asPoint . (calcPoint params)
      n' = n_integral params

-- Calculate the points along the sides of the target area
calcArea :: CalcParams -> ZPlanePoints
calcArea params = (
    (calcSide params (a' params) (b' params)),
    (calcSide params (b' params) (c' params)),
    (calcSide params (c' params) (d' params)),
    (calcSide params (d' params) (a' params))
    )

-- Representation of complex number as point (2-element tuple)
asPoint :: (RealFloat a) => Complex a -> (a, a)
asPoint u = (realPart u, imagPart u)

dzdu :: CalcParams -> Complex Double -> Complex Double
dzdu params u = ((dwdu params) u) * (((chi_0 params) u) + ((f_corr params) u))
