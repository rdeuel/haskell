import qualified System.Environment as Env
import Data.List

main = do  
   args <- Env.getArgs  
   progName <- Env.getProgName  
   putStrLn "The arguments are:"  
   mapM putStrLn args  
   putStrLn "The program name is:"  
   putStrLn progName  

