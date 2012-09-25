

countChange :: Int -> [Int] -> Int
countChange money coins@(currentCoin:otherCoins) = 
   let countChange' :: Int -> Int -> [Int] -> Int 
       countChange' _ currCount [] = currCount
       countChange' money currCount coins@(currentCoin:otherCoins)
        | currentCoin == money = countChange' money (currCount + 1) otherCoins
        | currentCoin > money = countChange' money currCount otherCoins
        | otherwise =
            let newCount = countChange' (money - currentCoin) currCount coins
            in countChange' money newCount otherCoins
   in countChange' money 0 coins

test1 = countChange 300 [5,10,20,50,100,200,500]
verify1 = test1 == 1022

test2 = countChange 301 [5,10,20,50,100,200,500]
verify2 = test2 == 0

test3 = countChange 300 [500,5,50,100,20,200,10] 
verify3 = test3 == 1022
