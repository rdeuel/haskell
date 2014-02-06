
import qualified System.Random as Rand
import Control.Monad

-- rndInt provides a specific type for random. Otherwise it's
-- random :: (RandomGen g, Random a) => g -> (a, g), which is ambiguous.
g0 = Rand.mkStdGen 100
rndInt :: Rand.StdGen -> (Int, Rand.StdGen)
rndInt = Rand.random

rndBool :: Rand.StdGen -> (Bool, Rand.StdGen)
rndBool = Rand.random

-- start with a gen, and get a bunch (infinite number) of objects
-- nice pattern for generating list where x_n = f (x_(n-1))
randoms gen0 = r0 : rs
   where 
      r0 = Rand.random gen0
      rs = randoms $ snd r0

rndBools :: Rand.StdGen -> [(Bool, Rand.StdGen)]
rndBools = randoms

rndInts :: Rand.StdGen -> [(Int, Rand.StdGen)]
rndInts = randoms

-- get a StdGen from the system (actually an IO StdGen)
iorands :: IO [(Int, Rand.StdGen)]
iorands = liftM randoms Rand.getStdGen

ten_iorands = liftM (take 10) iorands

