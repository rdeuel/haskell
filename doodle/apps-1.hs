

data MachineSpec = MachineSpec {mid :: Int}

create_machine :: MachineSpec -> IO ()
create_machine spec = writeFile (name ++ ".txt") name
   where name = "machine-" ++ (show $ mid spec)

specs = [MachineSpec n | n <- [0..]]

create_machines n = sequence $ take n $ map create_machine specs

--main = create_machines 4


