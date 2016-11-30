powerMod b 1 m = b `mod` m
powerMod b e m = ( b * (powerMod b (e-1) m)) `mod` m
main = do print (powerMod 3 3 10)
