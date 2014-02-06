divides d n = rem n d == 0

ldf k n | divides k n = k
        | k^2 > n = n
        | otherwise = ldf (k+1) n

ld n = ldf 2 n

prime0 n | n < 1 = error "not a positive integer"
         | n == 1 = False
         | otherwise = ld n == n

factors :: Integer -> [Integer]
factors n | n < 1 = error "argument not positive"
          | n == 1 = []
          | otherwise = p : factors (div n p) where p = ld n

