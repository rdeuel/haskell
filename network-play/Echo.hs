--module Echo where
import GHC.IO.Handle (hClose, hGetLine)
import System.IO (hPutStrLn)
import TcpServer
import Control.Concurrent


nextCmd hdl = do 
   l <- hGetLine hdl
   if (take 4 l) == "quit"
   then do 
      hPutStrLn hdl "done"
   else do
      hPutStrLn hdl $ "received " ++ l
      putStrLn $ "  " ++ l
      nextCmd hdl

main = TcpServer.start 8080 nextCmd


