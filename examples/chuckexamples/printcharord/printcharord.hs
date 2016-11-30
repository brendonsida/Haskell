import Data.Char
import Data.String

printElements [] = return ()
printElements (x:xs) = do print (ord x)
                          printElements xs
line = "test"
main = do
        printElements line

