module TcpServer where

import Control.Concurrent
import Control.Exception
import Network
import Network.Socket (sClose)
import GHC.IO.Handle (hClose)

start portnum handler = withSocketsDo $ do
   socket <- listenOn $ PortNumber portnum
   putStrLn $ "listening on port " ++ (show portnum)
   nextSocket socket `finally` sClose socket
   where
      nextSocket socket = do
         (peer, host, port) <- accept socket
         putStrLn $ "connected to " ++ host ++ ":" ++ (show port)
         forkIO $ handler peer `finally` hClose peer
         nextSocket socket

