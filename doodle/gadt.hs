-- exercise from http://en.wikibooks.org/wiki/Haskell/GADT
{--
{-- regular ADTs --}
data Expr = I Int
        | B Bool           -- boolean constants
        | Add Expr Expr
        | Mul Expr Expr
        | Eq  Expr Expr    -- equality test
 
eval :: Expr -> Maybe (Either Int Bool)
eval (I i) = Just $ Left i
eval (B b) = Just $ Right b
eval (Add e1 e2) = case (eval e1, eval e2) of
                     (Just (Left i), Just (Left j)) -> Just $ Left (i + j)
                     (_, _) -> Nothing
eval (Mul e1 e2) = case (eval e1, eval e2) of
                     (Just (Left i), Just (Left j)) -> Just $ Left (i * j)
                     (_, _) -> Nothing
eval (Eq e1 e2) = case (eval e1, eval e2) of
                     (Just (Left i), Just (Left j)) -> Just $ Right (i == j)
                     (Just (Right b), Just (Right c)) -> Just $ Right (b == c)
                     (_, _) -> Nothing
--}

{-- GADTs --}
{-#LANGUAGE GADTs #-}
data Expr a where
   I :: Int -> Expr Int
   B :: Bool -> Expr Bool
   Add :: Expr Int -> Expr Int -> Expr Int
   Mul :: Expr Int -> Expr Int -> Expr Int
   Eq :: (Eq a) => Expr a -> Expr a -> Expr Bool

eval :: Expr a -> a
eval (I n) = n
eval (B b) = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Eq  e1 e2) = eval e1 == eval e2


