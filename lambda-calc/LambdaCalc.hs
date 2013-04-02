
module LambdaCalc where

data Term = 
     Var String
   | Apply Term Term
   | Fun String Term

instance Show Term where 
   show (Var name) = name
   show (Apply f arg) = "(" ++ (show f) ++ (show arg) ++ ")"
   show (Fun varname body) = "\\" ++ varname ++ "." ++ (show body)
{-
instance Show Term where 
   show (Var name) = "(Var \"" ++ name ++ "\")"
   show (Apply f arg) = "(Apply " ++ (show f) ++ " " ++ (show arg) ++ ")"
   show (Fun varname body) = "(Fun \"" ++ varname ++ "\" " ++ (show body) ++ ")"
-}

reduce :: Term -> (Bool, Term)
reduce term =
   case (reduce' term) of
      (False, reduced) -> (False, reduced)
      (True, reduced) -> reduce reduced
   where 
      reduce' t =
         case t of
            (Var _) -> (False, t)
            (Fun farg body) -> (reduced, (Fun farg new_body))
               where (reduced, new_body) = reduce' body
            (Apply (Fun argname body) arg) -> (True, replace argname body arg)
               where
                  replace n (Var n') term = if n == n' then term else (Var n')
                  replace n (Fun arg body) term = Fun arg (replace n body term)
                  replace n (Apply t1 t2) term = Apply (replace n t1 term) (replace n t2 term)
            (Apply t1 t2) ->
               case (reduce' t1, reduce' t2) of
                  ((False, _), (False, _)) -> (False, t)
                  ((_, new_t1), (_, new_t2)) -> (True, Apply new_t1 new_t2)
   
zero = Fun "f" (Fun "x" (Var "x"))
one = Fun "f" (Fun "x" (Apply (Var "f") (Var "x")))
suc = Fun "n" (Fun "f" (Fun "x" (Apply (Var "f") (Apply (Apply (Var "n") (Var "f")) (Var "x")))))
 
