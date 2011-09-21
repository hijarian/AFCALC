import GHC.Conc
import System.Info
import Text.Printf
import Data.Version

main = do
  printf "Compiled with %s-%s on %s/%s\n" compilerName (showVersion compilerVersion) os arch 
  printf "Running with %d OS threads\n" numCapabilities 
