mygcd n1 0 = n1
mygcd n1 n2 = mygcd n2 (mod n1 n2)
main = do print(mygcd 60 48)
