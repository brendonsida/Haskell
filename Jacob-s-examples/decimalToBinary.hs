decToBin 0 = [0]
decToBin n = reverse (getBinDigit n)

getBinDigit 0 = []
getBinDigit n = let (q,r) = n `divMod` 2 in r : getBinDigit q

main = do print(decToBin 100)
