module StateAdd where

import Control.Monad.State
import qualified AddingMachine as AM

type Carrier = State Bool

-- objects of type Carrier a hold a function of type Bool -> (a, Bool)
type Adder a = a -> a -> Carrier a
addBase :: (Ord a, Num a) => a -> a -> a -> Carrier a
addBase base x y = state (AM.addBase base x y)

adds :: Adder a -> Adder [a]
adds = zipWithM



