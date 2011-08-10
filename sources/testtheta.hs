module Main where

import Theta
import Time
import Data.Complex

main = do
  time $ return $ foldl (+) (0 :+ 0) $ map ((theta1 20 0.75).(:+ 1)) [0..10000000000000000]
  return ()
