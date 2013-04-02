
module LambdaCalc where

data Term = 
     Var String
   | Apply Term Term
   | Fun String Term

{-
instance Show Term where 
   show (Var name) = name
   show (Apply f arg) = "(" ++ (show f) ++ (show arg) ++ ")"
   show (Fun varname body) = "\\" ++ varname ++ "." ++ (show body)
-}
instance Show Term where 
   show (Var name) = "(Var \"" ++ name ++ "\")"
   show (Apply f arg) = "(Apply " ++ (show f) ++ " " ++ (show arg) ++ ")"
   show (Fun varname body) = "(Fun \"" ++ varname ++ "\" " ++ (show body) ++ ")"


type Env = [(String, Term)]

reduce :: Term -> (Bool, Term)
reduce term = reduce' (True, term)
   where 
      reduce' (False, t) = (False, t)
      reduce' (True, t) =
         case t of
            (Var _) -> (False, t)
            (Fun farg body) ->
               case (reduce body) of
                  (False, _) -> (False, t)
                  (True, new_body) -> (True, (Fun farg new_body))
            (Apply (Fun argname body) arg) -> (True, replace argname body arg)
               where
                  replace n (Var n') term = if n == n' then term else (Var n')
                  replace n (Fun arg body) term = Fun arg (replace n body term)
                  replace n (Apply t1 t2) term = Apply (replace n t1 term) (replace n t2 term)
            (Apply t1 t2) ->
               case (reduce t1, reduce t2) of
                  ((False, _), (False, _)) -> (False, t)
                  ((_, new_t1), (_, new_t2)) -> (True, Apply new_t1 new_t2)
   
{-
reduce (Var name) = Var name
reduce (Fun argname (Var vname)) = reduce $ Fun argname (reduce body)
reduce (Fun argname body) = reduce $ Fun argname (reduce body)
reduce (Apply (Fun argname body) arg) = reduce $ replace argname body arg
   where
      replace n (Var n') term = if n == n' then term else (Var n')
      replace n (Fun arg body) term = Fun arg (replace n body term)
      replace n (Apply t1 t2) term = Apply (replace n t1 term) (replace n t2 term)
reduce (Apply t1 t2) = Apply (reduce t1) (reduce t2)
-}
zero = Fun "f" (Fun "x" (Var "x"))
one = Fun "f" (Fun "x'" (Apply (Var "f") (Var "x'")))
suc = Fun "n" (Fun "f" (Fun "x" (Apply (Var "f") (Apply (Apply (Var "n") (Var "f")) (Var "x")))))
--end = Fun "f" (Fun "x" (Apply (Var "f") (Apply (Fun "x" (Apply (Var "x") (Var "x")
     
{-
-- (\n.(\f.(\x.f n f x))) (\f.(\x'.x'))
-- (\f.(\x.f (\f.(\x'.x')) f x)))
-- (\f.(\x.f (\x'.x')))
-- \f.\x.(f((\f.\x.xf)x))

-- (Fun "f" (Fun "x" (Apply (Var "f") (Apply (Fun "x" (Var "x")) (Var "x")))))
Fun "f" 
(Fun "x" 
(Apply (Var "f") 
       (Apply (Apply (Fun "f" (Fun "x" (Var "x"))) 
                     (Var "f")) 
              (Var "x"))))

Apply (Var "f") (Apply (Apply (Fun "f" (Fun "x" (Var "x"))) 
                     (Var "f")) 
              (Var "x"))
Apply (Fun "f" (Fun "x" (Var "x"))) (Var "f")


-- (\f.(\x.f (\x'.x')))
-}
 
