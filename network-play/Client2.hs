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
import System.IO.Error

try = tryIOError

cleanLine h = do
   lineRes <- try $ hGetLine h
   case lineRes of
      Left error -> return $ Left error
      Right line -> return $ Right $ filter (/= '\r') line

parseStatus line =
   (name, (read code)::Int, dropWhile isSpace suffix2)
   where 
      (name, suffix1) = span (not . isSpace) line
      (code, suffix2) = span (not . isSpace) $ dropWhile isSpace suffix1

getStatus h = do
   lineRes <- cleanLine h
   case lineRes of
      Left error -> return $ Left error
      Right line -> do
         putStrLn $ "status = " ++ line
         return $ Right $ parseStatus line

readHeaders handle headers = do
   lineRes <- cleanLine handle
   case lineRes of
      Left error -> return $ Left error
      Right line -> do
         putStrLn $ "header line = " ++ line
         if (length line) == 0
         then return $ Right headers
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
         lineRes <- cleanLine h
         case lineRes of
            Left error -> return $ Left error
            Right line -> do
               chunkSize <- return $ readChunkSize line
               putStrLn $ "line = " ++ line ++ ", " ++ (show chunkSize)
               if chunkSize == 0
               then return $ Right mempty
               else do
                  chunkRes <- try $ ByteString.hGet h chunkSize
                  case chunkRes of
                     Left error -> return $ Left error
                     Right chunk -> do
                        hGetLine h -- crlf at end of chunk (no error check!)
                        restRes <- getBuilder
                        case restRes of   
                           Left error -> return $ Left error
                           Right rest -> return $ Right $ (lazyByteString chunk) `mappend` rest
   fmap (fmap toLazyByteString) getBuilder

getToEof h = do
   let
      getBuilder = do
         chunkRes <- try $ ByteString.hGet h 4096
         case chunkRes of
            Left error -> return $ Left error
            Right chunk -> do
               if (ByteString.length chunk) < 4096 
               then return $ Right $ lazyByteString chunk
               else do
                  restRes <- getBuilder
                  case restRes of
                     Left error -> return $ Left error
                     Right rest -> return $ Right $ (lazyByteString chunk) `mappend` rest
   fmap (fmap toLazyByteString) getBuilder

main = do
   hostname <- fmap head getArgs
   pageRes <- getPage hostname
   case pageRes of
      Left error -> putStrLn $ "ERROR: " ++ (show error)
      Right (status, headers, body) -> do
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
   connectRes <- try $ connectTo host (PortNumber 80)
   case connectRes of
      Left error -> do
         putStrLn "failed connect"
         return $ Left error
      Right h -> do
         putStrLn "connected"
         putRes <- try $ hPutStr h $ "GET / HTTP/1.1\r\nHost: " ++ host ++ "\r\n\r\n"
         case putRes of
            Left error -> return $ Left error
            Right () -> do
               putStrLn "sent request"
               statusRes <- getStatus h
               case statusRes of
                  Left error -> return $ Left error
                  Right status -> do
                     putStrLn $ show status
                     headersRes <- readHeaders h Map.empty
                     case headersRes of
                        Left error -> return $ Left error
                        Right headers -> do
                           putStrLn $ show headers
                           bodyRes <- case (Map.lookup "Transfer-Encoding" headers,
                                         Map.lookup "Content-Length" headers) of
                                        (Just _, _) -> getChunks h
                                        (Nothing, Just contentLen) -> try $ ByteString.hGet h ((read contentLen) :: Int)
                                        (Nothing, Nothing) -> getToEof h
                           case bodyRes of
                              Left error -> return $ Left error
                              Right body -> do
                                 hClose h
                                 return $ Right (status, headers, body)
   
   --c <- hGetContents h
   --putStrLn c
   --hClose h
   --putStrLn "handle closed"

