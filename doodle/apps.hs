{--
app = do
   worker1 `performs` mywebapp `using` [tomcat `for` container, mydb `for` database]
   worker1 `performs` tomcat
   worker2 `performs` mydb `using` postgres `for` rdbms
--}


import System.IO

data MachineSpec = MachineSpec {mid :: Integer} deriving Show

create_machine :: MachineSpec -> IO ()
create_machine spec = writeFile (name ++ ".txt") name
   where name = "machine" ++ (show $ mid spec)

count_from n = n : count_from (n + 1)

specs = [MachineSpec i | i <- count_from 0]

create_machines = map create_machine specs

-- take 5 create_machines




