-- Simplest possible area to test the charting library
module TestModel.Lines (
  pointlines
  ) where

import Data.Complex

pointlines = [
  [(0.0 :+ 0.0), (0.0 :+ 1.0), (0.0 :+ 2.0), (0.0 :+ 3.0)],
  [(0.0 :+ 3.0), (1.0 :+ 3.0), (2.0 :+ 3.0), (3.0 :+ 3.0)],
  [(3.0 :+ 3.0), (3.0 :+ 2.0), (3.0 :+ 1.0), (3.0 :+ 0.0)],
  [(3.0 :+ 0.0), (2.0 :+ 0.0), (1.0 :+ 0.0), (0.0 :+ 0.0)]
  ]

