import Control.Monad

-- main = putStrLn "hello, mother fucker"
main1 = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")

main2 = (putStrLn "Hello, what's your name?") >>
        getLine >>=
        \name -> putStrLn ("Hey " ++ name ++ ", you rock!")

-- many ways to modify the result of getline before passing it to putStrLn 
main3 = (putStrLn "Hello, what's your name?") >>
        getLine >>=
        \input -> ((return (input ++ "ZZZZZ")) >>=
        \name -> putStrLn ("Hey " ++ name ++ ", you rock!"))
 
main4 = (putStrLn "Hello, what's your name?") >>
        (liftM (++ "YYYY") getLine) >>=
        \name -> putStrLn ("Hey " ++ name ++ ", you rock!")
 
main5 = do
    putStrLn "Hello, what's your name?"  
    name <- liftM (++ "WWW") getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")

-- this one uses syntactic sugar to the fullest and is the easiest to read
main6 = do
    putStrLn "Hello, what's your name?"  
    name <- getLine
    let name' = "QQQ" ++ name
    putStrLn ("Hey " ++ name' ++ ", you rock!")

main = main6

-- m1 is a monadic VALUE. In both cases, extract the result of
-- running m1, call the function with the result as an arg
-- and wrap it up in the monad
liftM' f m1 = do { x1 <- m1; return (f x1) }

liftM'' f m1 = m1 >>= \x1 -> return $ f x1
