import Data.Map.Lazy
import Control.Monad.Reader
import Data.Functor.Identity

-- work with an environment
addFromEnv :: String -> String -> (Map String Integer) -> Integer
addFromEnv name1 name2 env = (env ! name1) + (env ! name2)
