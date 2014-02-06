import Control.Monad.Writer
import Data.Functor.Identity

-- log arithmetic operations
multiply y x = (r, " x " ++ (show y) ++ " = " ++ (show r))
   where r = x * y

add y x = (r, " + " ++ (show y) ++ " = " ++ (show r))
   where r = x + y

-- this is like bind
applyLog (val, log) f = (result, log `mappend` stmt)
   where (result, stmt) = f val

-- this is like return (except Writer's return doesn't include a log)
startWith x = (x, show x)

logg stmt x = (x, stmt)

exp = startWith 3 `applyLog` multiply 2 `applyLog` add 4

-- now with the WriterT monad
-- the WriterT monad constructor is :: m (a, log) -> WriterT log m a
mkWriter f = WriterT (Identity f)
sw x = mkWriter (startWith x)
mlt y x = mkWriter (multiply y x)
ad y x = mkWriter (add y x)
lo stmt x = mkWriter (logg stmt x)
op = sw 2 >>= (lo "fuck you") >>= mlt 3 >>= ad 4
-- then run it with runWriter op
--
-- similarly:
opdo = do
   x <- sw 2       -- x is 2
   y <- mlt 3 x    -- y is 6
   ad 4 y          -- result is 10

-- difference lists
dlist l = \xs -> l ++ xs
