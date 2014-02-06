import Control.Exception
import Data.Typeable

data ClientException e = FailedConnect String e
                         | BadResponse String e
                         | BadHeaders String e
                         | BadRead String e
                           deriving Typeable

instance (Show e, Typeable e) => Exception (ClientException e)

instance Show e => Show (ClientException e) where
   show (FailedConnect msg e) = msg ++ " Failed to connect: " ++ (show e)
   show (BadResponse msg e) = msg ++ " Failed reponse code: " ++ (show e)
   show (BadHeaders msg e) = msg ++ " Unable to parse headers: " ++ (show e)
   show (BadRead msg e) = msg ++ " Could not read page: " ++ (show e)
