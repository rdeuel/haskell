module HttpParse {--(
   Method,
   Get,
   Post,
   Put,
   Delete,
   Head,
   Connect,
   parseReq
) --}where
import Data.Char
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map

data Method = Get | Post | Put | Delete | Head | Connect deriving Show

parseReq :: String ->
            Either ParseError 
                   (Method, String, (Map.Map String String), String)
parseReq input = runParser requestLine Map.empty "request" input

requestLine = do
   m <- method
   skipMany $ (oneOf " \t")
   p <- path
   prm <- params
   v <- version
   return (m, p, prm, v)

method :: GenParser Char st Method
method = do
   txt <- many $ noneOf " \t"
   case (map toLower txt) of
      "get" -> return Get
      "post" -> return Post
      "put" -> return Put
      "delete" -> return Delete
      "head" -> return Head
      "connect" -> return Connect
      _ -> fail $ "invalid method " ++ txt

path :: GenParser Char st String
path = many (noneOf "? \t")

param :: GenParser Char st (String, String)
param = do
   name <- many $ noneOf "="
   char '='
   val <- many $ noneOf " &"
   return (name, val)

params :: GenParser Char (Map.Map String String) (Map.Map String String)
params =
   (char '?' >> nextParam) <|> (oneOf " \t" >> getState)
   where
      nextParam = do
         (name, val) <- param
         updateState $ \ps -> Map.insert name val ps
         (oneOf " \t" >> getState) <|> (char '&' >> nextParam)

version :: GenParser Char st String
version = do
   ver <- many $ alphaNum <|> oneOf "/."
   eof
   return ver


