import qualified Data.Foldable as F

data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show, Read, Eq)

--instance F.Foldable Tree where
tfoldl f acc Empty = acc
tfoldl f acc (Node val left right) = tfoldl f (tfoldl f (f acc val) left) right

tfoldr f acc Empty = acc
tfoldr f acc (Node val left right) = f val (tfoldr f (tfoldr f acc right) left)

instance F.Foldable Tree where  
   foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
   foldMap f Empty = mempty  
   foldMap f (Node x l r) = F.foldMap f l `mappend` f x `mappend` F.foldMap f r

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


test_tree_2 = Node True
                   (Node False
                        (Node True Empty (Node True Empty Empty))
                        Empty
                   )
                   (Node True
                        Empty
                        (Node True
                             (Node False Empty Empty) 
                             Empty))

tall = tfoldl (&&) True
tany = tfoldl (||) False

