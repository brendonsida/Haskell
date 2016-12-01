--This doesnt work because it expects all of the parameters
--to have the same type
f :: a -> a -> a
--f :: Num a => a -> a -> a -- would work becaue now all of 
-- the expected values are numerals
f x y = x + y
main =  print (f 2 2.3)
