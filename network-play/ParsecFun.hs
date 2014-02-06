import Text.ParserCombinators.Parsec

name = many (noneOf ": ") :: GenParser Char st String
val = many (noneOf "\r\n") :: GenParser Char st String

parseNV s = parse (name >>= \n -> val >>= \v -> return (n, v)) "xxx" s
test = parseNV "name: value"
