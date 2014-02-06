import Control.Concurrent
import Control.Exception

trySome :: IO a -> IO (Either SomeException a)
trySome = try

main =
   res :: 
   res <- try $ threadDelay 30000000
   case res of
      Right () -> putStrLn "done" 
      Left e -> do
         putStrLn $ "exception: " ++ (show e)
         res <- try $ threadDelay 30000000
         putStrLn $ show res
