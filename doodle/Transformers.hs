-- code from http://www.grabmueller.de/martin/www/pub/Transformers.en.html
-- 'Monad Transformers Step by Step'

module Transformers where
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map.Lazy as Map

type Name = String -- variable names
data Exp = Lit Integer -- expressions
         | Var Name
         | Plus Exp Exp
         | Abs Name Exp
         | App Exp Exp
         deriving (Show )
data Value = IntVal Integer -- values
           | FunVal Env Name Exp
           deriving (Show )
type Env = Map.Map Name Value -- mapping from names to values

eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2) =
   let IntVal i1 = eval0 env e1
       IntVal i2 = eval0 env e2
   in  IntVal (i1 + i2 )
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) =
   let val1 = eval0 env e1
       val2 = eval0 env e2
   in case val1 of
         FunVal env' n body -> eval0 (Map.insert n val2 env') body

-- 12 + ((\x -> x)(4 + 2))
exampleExp = (Lit 12) `Plus` (App (Abs "x" (Var "x")) ((Lit 4) `Plus` (Lit 2)))
exampleEval0 = eval0 Map.empty exampleExp

-- convert the eval function to monadic style using the Identity Monad
type Eval1 a = Identity a
runEval1 :: Eval1 a -> a
runEval1 ev = runIdentity ev

-- Based on the Eval1 monad, we now rewrite the eval0 function as eval1.
-- Note that the Eval1 monad is just the Identity monad
eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = case (Map.lookup n env) of
                        Just v -> return v
                        Nothing -> fail "not found"
eval1 env (Plus e1 e2) = do IntVal i1 <- eval1 env e1
                            IntVal i2 <- eval1 env e2
                            return $ IntVal (i1 + i2)
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) = do val1 <- eval1 env e1
                           val2 <- eval1 env e2
                           case val1 of
                              FunVal env' n body -> eval1 (Map.insert n val2 env') body

exampleEval1 = runEval1 (eval1 Map.empty exampleExp)

