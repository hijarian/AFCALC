module Main where

import Theta
import Time
import Data.Complex
import Test.QuickCheck


e :: RealFloat a => a
e = 0.0001 -- 5 digits after dot will be sufficient

n = 20 -- DO NOT MAKE IT VERY LARGE!

tau :: RealFloat a => a
tau = 0.3

q :: RealFloat a => Complex a
q = qpar tau

prop_Th2Th1 :: (Arbitrary a, RealFloat a) => Complex a -> Property
prop_Th2Th1 u = 
  forAll ( arbitrary `suchThat` (\u -> (< pi) . realPart . abs $ u) ) $ 
  \u -> (< e) . magnitude $ (theta2 n q u) - (theta1 n q v)
  where v = u + (pi/2 :+ 0)

prop_Th3Th4 :: (Arbitrary a, RealFloat a) => Complex a -> Bool
prop_Th3Th4 u = (< e) . magnitude $ th4 - th3
  where a = pi/2 :+ 0
        th3 = theta3 n q u
        th4 = theta4 n q (u + a)
  
prop_SignFun x = signfun x == ( (:+ 0) . (** fromInteger x) $ (-1.0) )
  where types = x::Integer
 
-- Testing qpar function
-- precondition is that qpar's argument should be > 0 (line @1)
-- condition is that real part of absolute value of qpar must be < 1 (line @2)
prop_QPar :: (RealFloat a) => a -> Property 
prop_QPar tau = 
  forAll ( arbitrarySizedFractional `suchThat` (\tau -> tau > 0) ) $ -- @1
  \tau -> ( (< 1) . realPart . abs . qpar $ tau )                    -- @2
  
main = do
  time $ return $ foldl (+) (0 :+ 0) $ map ((theta1 20 0.75).(:+ 1)) [0..10000000000000000]
  return ()
