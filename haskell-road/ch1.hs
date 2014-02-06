bang :: String -> String
bang s = 
   let bang' [] _ _ = []
       bang' (c:cs) n m | m == 0 = bang' cs (n + 1) (n + 1)
                        | otherwise = c:(bang'  (c:cs) n (m - 1))
   in bang' s 1 1
