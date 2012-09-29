module Sets where

-- the Set type contains the characteristic function
data Set a = MkSet {contains :: a -> Bool}
singletonSet :: (Eq a) => a -> Set a
singletonSet x = MkSet $ \elem -> elem == x

union :: (Eq a) => Set a -> Set a -> Set a
union s1 s2 = MkSet $ \elem -> (contains s1 elem) || (contains s2 elem)

intersect :: (Eq a) => Set a -> Set a -> Set a
intersect s1 s2 = MkSet $ \elem -> (contains s1 elem) && (contains s2 elem)

-- univ is a 'universe' of possible values for set
toList :: (Ord a, Enum a) => [a] -> (Set a) -> [a]
toList univ set = [x | x <- univ, contains set x]
toListInts = toList [-100..100]

-- universal quantifier
forall :: (Ord a, Enum a) => [a] -> (a -> Bool) -> Set a -> Bool
--forall univ pred set = foldl (\x y -> x && (pred y)) True (toList univ set)
forall [] _ _ = True
forall (univ@curr:rest) pred set
   | (contains set curr) && not (pred curr) = False
   | otherwise = forall rest pred set
forallInt = forall [-100..100]

-- existential quantifier
exists :: (Ord a, Enum a) => [a] -> (a -> Bool) -> Set a -> Bool
exists univ pred set = not $ forall univ (not.pred) set
existsInt = exists [-100..100]

-- this is very slow
smap :: (Ord a, Enum a, Ord b) => [a] -> (a -> b) -> Set a -> Set b
smap univ f set = MkSet $ \mapped -> exists univ (\elem -> f elem == mapped) set
smapInt f set = smap [-100..100] f set

--instance Show a => Show (Set a) where
--   (MkSet _) = "{}"
--


showSet :: (Set Integer) -> String
showSet set =
   let lbound = (-1000)
       ubound = 1000
       showSet' i acc set
         | i > ubound = acc
         | i == ubound = showSet' (i + 1) (acc ++ "}") set
         | i < ubound && (contains set i) = showSet' (i + 1) (acc ++ (show i) ++ ",") set
         | otherwise = showSet' (i + 1) acc set
   in showSet' lbound "{" set


-- tests --
s0 = singletonSet 0
s1 = singletonSet 1
s2 = singletonSet 2
s3 = singletonSet 3
s4 = singletonSet 4
s5 = singletonSet 5
s012345 = union s0 (union s1 (union s2 (union s3 (union s4 s5))))

-- True
t1 = contains s0 0

-- False
t2 = forallInt  (\x -> x > 5) s012345

-- True
t3 = forallInt  (\x -> x <= 5) s012345

-- False
t4 = forallInt  (\x -> x <= 0) s012345

--True
t5 = forallInt  (\x -> x >= 0) s012345

-- True
t6 = existsInt (\x -> x < 2) s1

