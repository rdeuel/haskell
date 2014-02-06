module HttpServer where
import Control.Monad
import Data.Char
import Data.Monoid
import Data.Text (pack)
import Data.Text.Read (hexadecimal)
import GHC.IO.Handle (hClose, hGetLine, Handle, hPutStr)
import System.IO (hPutStrLn)
import TcpServer
import Control.Concurrent
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as ByteString
import Data.ByteString.Lazy.Builder

import HttpParse

data Request = Request {
   method :: Method,
   path :: String,
   params :: Map.Map String String,
   httpVersion :: String,
   headers :: Map.Map String String,
   body :: ByteString.ByteString
} deriving Show

cleanLine :: Handle -> IO String
cleanLine = (fmap (filter (/= '\r'))) . hGetLine

readHeaders handle headers = do
   line <- cleanLine handle
   if (length line) == 0
   then return headers
   else do
      let (name, rest) = span (/= ':') line
          value = dropWhile (not . isAlphaNum) rest
      readHeaders handle (Map.insert name value headers)

getBody h headers =
   case (Map.lookup "Transfer-Encoding" headers,
         Map.lookup "Content-Length" headers) of
         (Just _, _) -> getChunks h
         (Nothing, Just contentLen) -> ByteString.hGet h ((read contentLen) :: Int)
         (Nothing, Nothing) -> getToEof h

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

readChunkSize :: String -> Int
readChunkSize s = case (hexadecimal $ pack s) of
   Right (i, _) -> i
   _ -> 0

handler mkResponse hdl = do 
   l <- cleanLine hdl
   req <- return $ parseReq l
   case req of
      Left error -> hPutStr hdl $ "HTTP/1.0 400 Bad Request: " ++ (show error) ++ "\r\n"
      Right (method, path, params, ver) -> do
         headers <- readHeaders hdl Map.empty
         body <- case method of
            Post -> getBody hdl headers 
            Put -> getBody hdl headers 
            _ -> return ByteString.empty 
         request <- return $ Request method path params ver headers body
         putStrLn $ show request
         hPutStr hdl $ mkResponse request

start port genResponse = TcpServer.start port (handler genResponse)


