module ReverseComplexGraph.Time ( 
  time
  ) where

import Text.Printf
import Control.Exception
import System.CPUTime

-- Simplest timing function

-- To time the pure function running time, call `time` as follows:
--     time $ pure_function args `seq` return ()

time :: IO t -> IO t
time a = do
  start <- getCPUTime
  v <- a
  end   <- getCPUTime
  let diff = (fromIntegral (end - start)) / ticksInSecond
  printf "Computation time: %0.3f sec\n" (diff :: Double)
  return v

ticksInSecond = 10^12
