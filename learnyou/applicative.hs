import Control.Applicative

e = pure (++)  <*> ["hello", "goodbye"] <*> ["Bob", "asshole"]

p1 = fmap (++) ["hello", "goodbye"]

p2 = p1 <*> ["Bob", "asshole"]

ioa = (++) <$> (return "hello ") <*> getLine 
