mymax' :: (Ord a) => [a] -> a  
mymax' [] = error "EMPTY"  
mymax' [x] = x  
mymax' (x:xs)   
        | x > maxTail = x  
        | otherwise = maxTail  
        where maxTail = mymax' xs 
main = do{ 
        print (mymax' [1, 2, 3]);
        print (mymax' [3, 2, 1]);
        }
