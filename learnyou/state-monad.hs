import Control.Monad.State.Lazy
import Data.Functor.Identity
import qualified System.Random as Rand
import Control.Monad

type Stack a = [a]
pop :: Num a => Stack a -> (a, Stack a)
pop [] = (0, [])
pop (x:xs) = (x, xs)
push :: a -> Stack a -> ((), Stack a)
push x xs = ((), x:xs)

-- 
type StackManip a = StateT [a] Identity a
popm = StateT (Identity . pop)
pushm x = StateT (Identity . (push x))

-- same as runState action initial_state
one_push = runIdentity $ runStateT (pushm 5) []

-- ridiculous way to reverse a list using StateT
revers xs = do
   pushall xs
   popall []

pushall xs =
   case xs of
      [] -> return ()
      x : xs -> pushm x >> pushall xs

popall curr = do
   x <- popm
   if x == 0
   then return curr
   else popall $ curr ++ [x]
   
run_reverse xs = runState (revers xs) []


-- rndInt provides a specific type for random. Otherwise it's
-- random :: (RandomGen g, Random a) => g -> (a, g), which is ambiguous.
g0 = Rand.mkStdGen 100
rndInt :: Rand.StdGen -> (Int, Rand.StdGen)
rndInt = Rand.random
rndState = StateT $ Identity . rndInt

rands n = do
   if n == 0
   then return []
   else do
      r <- rndState
      liftM (r:) (rands (n - 1))

run_rand n = runState (rands n) g0
      
 
   
