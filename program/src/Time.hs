module Time where
import Text.Printf
import Control.Exception
import System.CPUTime

-- Функция для оценки того, сколько времени работает та или иная функция

-- Для оценки того, сколько будет работать чистая функция, вызываем следующим образом:
-- time $ pure_function args `seq` return ()

time :: IO t -> IO t
time a = do
  start <- getCPUTime
  v <- a
  end   <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  printf "Computation time: %0.3f sec\n" (diff :: Double)
  return v
