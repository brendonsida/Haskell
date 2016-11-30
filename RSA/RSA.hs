import Data.Char (ord)
import Data.Char (chr)

pmod a 1 n = mod a n 
pmod a b n = (a*(pmod a (b-1) n)) `mod` n

encrypt m e n = pmod (asciipad m) e n

decrypt m t n = asciidpad(pmod m t n)

asciipad s
  |((length s) == 0) =  0 
  |otherwise = ((100 + toInteger((ord (head s)))) *(toInteger(1000 ^((length s)-1))))+(toInteger(asciipad (drop 1 s)))

asciidpad i
  | i <= 100 = ""
  | otherwise =(asciidpad (div i 1000)) ++ [chr((fromInteger(i `mod` 1000))-100)]
  
  
main = do 	print (encrypt "Hi" 149 248922799)
		print (decrypt (encrypt "Hi" 149 248922799) 10022429  248922799)