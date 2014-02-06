
-- note that (->) is a type constructor that takes two arguments
f :: (->) Int Int
f = (*2)

-- and (->) r is a Functor, so I can fmap the result eg
g :: Int -> String    -- the more normal way to write the type
g = fmap show f       -- return the string value of the result

-- so f 2 = 4, and g 2 = "4"
-- actually, it's just this: g = show . f, and fmap for functions
-- is just (.)
--

-- dumb_joke is a pure functure, fmap calls it inside the io context
dumb_joke = (++ " between the sheets")
dumb_joke_io = fmap dumb_joke getLine
