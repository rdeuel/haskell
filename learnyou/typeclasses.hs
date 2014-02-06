-- from http://learnyouahaskell.com/making-our-own-types-and-typeclasses
--
import qualified Data.Map.Lazy as Map

class YesNo a where
   yesno :: a -> Bool

-- note Int in this case should be thought of as a type constructor
-- with no parameter needed to make it concrete
instance YesNo Int where
   yesno = (/= 0)

instance YesNo [a] where
   yesno [] = False
   yesno _ = True

instance YesNo Bool where
   yesno = id

instance YesNo (Maybe a) where
   yesno (Just _) = True
   yesno Nothing = False

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance Functor Tree where
   fmap f Empty = Empty
   fmap f (Node val left right) = Node (f val) (fmap f left) (fmap f right)

test_tree = Node 1 
                 (Node 2 
                       (Node 3 Empty Empty) 
                       Empty
                 ) 
                 (Node 4 
                       Empty 
                       (Node 5 
                             (Node 6 Empty Empty) 
                             Empty))


-- emphasizing that ctr is a type constructor, a, b, (ctr a) and (ctr b) are
-- concrete types.
class Functor' ctr where
   fmap' :: (a -> b) -> ctr a -> ctr b

-- note that (Map k) is a type constructor, (Map k v) is a concrete type
instance Functor' (Map.Map k) where
   fmap' = Map.map

test_map = Map.fromList [('a', 1), ('b', 2), ('c', 3), ('d', 4)]
-- fmap' (+1) test_map

class DoubleFunctor ctr where
   dfmap :: (a -> b) -> (c -> d) -> (ctr a c) -> (ctr b d)

instance DoubleFunctor Either where
   dfmap f _ (Left x) = Left $ f x
   dfmap _ g (Right x) = Right $ g x

decide = dfmap (++ " Bob") (++ " Fucker")
t1 = decide (Left "Hello")
t2 = decide (Right "Goodbye")

