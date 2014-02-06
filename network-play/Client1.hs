module Client1 where
import Network
import GHC.IO.Handle
import Data.Char
import Data.List.Split
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as ByteString
import Data.ByteString.Lazy.Builder
import Data.Text (pack)
import Data.Text.Read
import System.Environment
import Control.Exception

cleanLine = (fmap (filter (/= '\r'))) . hGetLine
parseStatus line =
   (name, (read code)::Int, dropWhile isSpace suffix2)
   where 
      (name, suffix1) = span (not . isSpace) line
      (code, suffix2) = span (not . isSpace) $ dropWhile isSpace suffix1

getStatus h = do
   line <- cleanLine h
   putStrLn $ "status = " ++ line
   return $ parseStatus line

readHeaders handle headers = do
   line <- cleanLine handle
   putStrLn $ "header line = " ++ line
   if (length line) == 0
   then return headers
   else do
      let (name, rest) = span (/= ':') line
          value = dropWhile (not . isAlphaNum) rest
      readHeaders handle (Map.insert name value headers)

readChunkSize :: String -> Int
readChunkSize s = case (hexadecimal $ pack s) of
   Right (i, _) -> i
   _ -> 0

getChunks h = do
   let getBuilder = do
         line <- cleanLine h
         chunkSize <- return $ readChunkSize line
         putStrLn $ "line = " ++ line ++ ", " ++ (show chunkSize)
         if chunkSize == 0
         then return mempty
         else do
            chunk <- ByteString.hGet h chunkSize
            hGetLine h -- crlf at end of chunk
            fmap (mappend $ lazyByteString chunk) getBuilder
   fmap toLazyByteString getBuilder

getToEof h = do
   let
      getBuilder = do
         chunk <- ByteString.hGet h 4096
         if (ByteString.length chunk) < 4096 
         then return $ lazyByteString chunk
         else fmap (mappend $ lazyByteString chunk) getBuilder
   fmap toLazyByteString getBuilder

main = do
   hostname <- fmap head getArgs
   (status, headers, body) <- getPage hostname
   putStrLn $ show status
   putStrLn $ show headers
   putStrLn $ show body
{--
main = do
   hostname <- fmap head getArgs
   getItAll hostname
--}
getItAll host = withSocketsDo $ do
   h <- connectTo host (PortNumber 80)
   putStrLn "connected"
   hPutStr h $ "GET / HTTP/1.1\r\nHost: " ++ host ++ "\r\n\r\n"
   putStrLn "sent request"
   contents <- hGetContents h
   putStrLn contents

getPage host = withSocketsDo $ do
   h <- connectTo host (PortNumber 80)
   putStrLn "connected"
   hPutStr h $ "GET / HTTP/1.1\r\nHost: " ++ host ++ "\r\n\r\n"
   putStrLn "sent request"
   status <- getStatus h
   putStrLn $ show status
   headers <- readHeaders h Map.empty
   putStrLn $ show headers
   body <- case (Map.lookup "Transfer-Encoding" headers,
                 Map.lookup "Content-Length" headers) of
      (Just _, _) -> getChunks h
      (Nothing, Just contentLen) -> ByteString.hGet h ((read contentLen) :: Int)
      (Nothing, Nothing) -> getToEof h
   hClose h
   return (status, headers, body)
   
   --c <- hGetContents h
   --putStrLn c
   --hClose h
   --putStrLn "handle closed"

