module AFCalc where
-- Module for getting actual points using functions defined in model

-- We use complex numbers
import Data.Complex

-- We use custom integrators written specifically for needs of AFCALC
-- EDIT @ 2011-09-26: Cabalized as complex-integrate package
import Data.Complex.Integrate

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
    -- Slot for user-defined full function in some simplified form maybe
    -- Will be used when is_simple flag equals True
    _dzdu       :: (Complex Double -> Complex Double),
    -- Slots for functions provided by user from his model
    _dwdu       :: (Complex Double -> Complex Double),
    _chi_0      :: (Complex Double -> Complex Double),
    _f_corr     :: (Complex Double -> Complex Double),
    -- Points at A, B, C and D, respectively
    points     :: (Complex Double, Complex Double, Complex Double, Complex Double), 
    -- Tau parameter, underscored to avoid name clash with model module
    -- WARNING: should be equal to the tau parameter fed to the dwdu, chi_0 and 
    -- f_corr functions!
--     _tau        :: Double,
    -- Frequency of discretization when integrating along lines
    n_integral :: Integer,
    -- Origin point to integration
    origin     :: Complex Double,
    -- Folder to save images to
    folder     :: String,
    -- Is the model simplified and contains only the dzdu function?
    is_simple  :: Bool 
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
-- As base point of integration we use point A
calcPoint :: CalcParams -> Complex Double -> Complex Double
calcPoint params u = integrate (dzdu' params) n' (origin params) u
    where n' = n_integral params

-- Calculate the line in target area corresponding to the line between
--  given points at source area
calcSide :: CalcParams -> Complex Double -> Complex Double -> PointList
calcSide params u1 u2 = map constructPoint pointlist
    where
      constructPoint = asPoint . (calcPoint params)
      n' = n_integral params
      pointlist = map ((+ u1) . (* h) . fromInteger) [0..n']
      h = (u2 - u1) / (fromInteger n')

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

-- Main function for calcPoint
-- User of library is expected to fill the dwdu, chi_0 and f_corr fields
--   of CalcParams with functions from his model
dzdu' :: CalcParams -> Complex Double -> Complex Double
dzdu' cpar u 
    | (is_simple cpar) == True = (dzdu' u)
    | otherwise                = (dwdu' u) * exp ( (f_corr' u) - (chi_0' u) )
        where
            dzdu'   = (_dzdu   cpar)
            dwdu'   = (_dwdu   cpar)
            f_corr' = (_f_corr cpar)
            chi_0'  = (_chi_0  cpar)
