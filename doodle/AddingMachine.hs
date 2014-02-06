module AddingMachine where

type Adder a = a -> a -> Bool -> (a, Bool)

-- make an adder given the base
addBase :: (Num a, Ord a) => a -> Adder a
addBase base a b carry = 
   let sum = a + b + (if carry then 1 else 0)
   in  if sum >= base
       then (sum - base, True)
       else (sum, False)

add10 = addBase 10

-- head is the least significant digit
adds1 :: [Integer] -> [Integer] -> [Integer]
adds1 ls rs =
   let
     adds' ls' rs' carry =
       case (ls', rs') of
         ([], []) -> []
         (l:ls'', []) -> let (n, c) = (add10 l 0 carry) in (n : adds' ls'' [] c)
         ([], r:rs'') -> let (n, c) = (add10 0 r carry) in (n : adds' [] rs'' c)
         (l:ls'', r:rs'') -> let (n, c) = (add10 l r carry) in (n : adds' ls'' rs'' c)
   in
     adds' ls rs False

adds2 :: Adder Integer -> [Integer] -> [Integer] -> [Integer]
adds2 adder ls rs =
   let
     adds' [] [] _ = []
     adds' (l:ls') [] carry = let (n, c) = (adder l 0 carry) in (n : adds' ls' [] c)
     adds' [] (r:rs') carry = let (n, c) = (adder 0 r carry) in (n : adds' [] rs' c)
     adds' (l:ls') (r:rs') carry = let (n, c) = (adder l r carry) in (n : adds' ls' rs' c)
   in
     adds' ls rs False


adds :: Adder a -> Adder [a]
adds _ [] [] c = ([], c)
adds adder (l:ls) (r:rs) c = (hd:tl, c'')
   where
      (hd, c') = adder l r c
      (tl, c'') = adds adder ls rs c'
adds _ _ _ _ = error "need same number of digits"

adds3 :: Adder a -> Adder [a]
adds3 adder xs ys c = foldr f ([], c) (zip xs ys)
   where
      f (x, y) (rs, c') = (hd:rs, c'')
         where (hd, c'') = adder x y c'

addsP add as bs i = (cs,last is)
 where
   (cs,os) = unzip (zipWith3 add as bs is)
   is = i : os


