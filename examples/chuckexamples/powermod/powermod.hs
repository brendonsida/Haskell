--powerMod b 1 m = b `mod` m
--powerMod b e m = ( b * (powerMod b (e-1) m)) `mod` m
powermod b e m
    | e == 1 b `mod` m
    | otherwise = powerTail
    where powerTail = ( b * (powerMod b (e-1) m)) `mod` m

--mymax' (x:xs)   
--        | x > maxTail = x  
--        | otherwise = maxTail  
--        where maxTail = mymax' xs 

main = do print (powermod 3 3 10)
