module AFCalc where
-- Module for getting actual points using functions defined in model
-- It uses two items from model:
-- 1. Function dzdu param u
-- 2. Data type ModelParams

-- ModelParams should have "n_integral" method
-- TODO: make public interface for ModelParams

-- We use complex numbers
import Data.Complex

-- We use BlastModel.Full as our model providing ModelParams and dzdu function
-- HINT: HERE WE IMPORT OUR CUSTOM BUILT MODULE WITH DEFINITIONS FOR FUNCTIONS REPRESENTING OUR PROBLEM
-- Every other module, including this, are supplementary to calculate results
-- TODO: WARNING: Invert this process! AFCALC should be the library, included in other projects (which define dzdu, or, probably, dwdu, chi0 and f_corr, and which define ModelParams), and not a program including concrete definitions for problems as libraries!
--import BlastModel.Full
import BlastModel.ExpTest

-- We use custom integrators written specifically for needs of AFCALC
import AFCalc.Integrators

----------------------------------------
-- 1. Вычисление координат точек на границе воронки взрыва BEGIN

-- Making full list of Z points along lines DC, CB, BA, AD in that exact order
-- We need to preserve order because we calculate points by integration
zlist param = (
  (zlistDC param),
  (zlistCB param),
  (zlistBA param),
  (zlistAD param)
  )

-- Coordinates of points of rectangular area of var U
-- TODO: it should be defined in model!
d' = (pi/4, 0)
c' = (0, 0)
b' param = (0, (pi/4)*(tau param))
a' param = (pi/4, (pi/4)*(tau param))

-- List of points on sides at Z corresponding to sides at U

zlistDC param = zlistHorizontal param dx cx cy 
  where (dx, _) = d'
        (cx, cy) = c'

zlistCB param = zlistVertical   param cy by cx
  where (cx, cy) = c'
        (_, by) = b' param
        
zlistBA param = zlistHorizontal param bx ax ay
  where (ax, ay) = a' param
        (bx, _) = b' param
        
zlistAD param = zlistVertical    param ay dy ax
  where (ax, ay) = a' param
        (_, dy) = d'

-- Representation of complex number as point (2-element tuple)
asPoint :: (RealFloat a) => Complex a -> (a, a)
asPoint u = (realPart u, imagPart u)

-- Construct point at vertical line between (<x>, <y0>) and (<x>, <y>)
-- by integration and return tuple (<x>, <y>)
zVertical ::  ModelParams -> Double -> Double -> Double -> Complex Double
zVertical param x y0 y  = integrateVertical (dzdu param) n' y0 y x 
  where n' = n_integral param

-- Making points along vertical line between (<x>, <ya>) and (<x>, <yb>)
zlistVertical ::  ModelParams -> Double -> Double -> Double -> [(Double, Double)]
zlistVertical param ya yb x = map constructPoint $ quantize ya yb n'
  where
    constructPoint = asPoint . (zVertical param x ya)
    n' = n_integral param

-- Construct point at horizontal line between (<x0>, <y>) and (<x>, <y>)
-- by integration and return tuple (<x>, <y>)
zHorizontal ::  ModelParams -> Double -> Double -> Double -> Complex Double
zHorizontal param y x0 x  = integrateVertical (dzdu param) n' x0 x y 
  where n' = n_integral param

-- Making points along horizontal line between (<xa>, <y>) and (<xb>, <y>)
zlistHorizontal ::  ModelParams -> Double -> Double -> Double -> [(Double, Double)]
zlistHorizontal param xa xb y = map constructPoint $ quantize xa xb n'
  where
    constructPoint = asPoint . (zHorizontal param y xa)
    n' = n_integral param

