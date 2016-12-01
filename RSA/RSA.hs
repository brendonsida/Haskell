import Data.Char (ord)
import Data.Char (chr)
import System.Random
import Math.NumberTheory.Primes (isPrime)
import Math.NumberTheory.Moduli (invertMod)
import Math.NumberTheory.Moduli (powerModInteger)
import Data.Maybe
import Debug.Trace
import System.Environment (getArgs)


encrypt m e n = powerModInteger (asciipad m) e n

decrypt m t n = asciidpad(powerModInteger m t n)

asciipad s
  |((length s) == 0) =  0 
  |otherwise = ((100 + toInteger((ord (head s)))) *(toInteger(1000 ^((length s)-1))))+(toInteger(asciipad (drop 1 s)))

asciidpad i
  | i <= 100 = ""
  | otherwise =(asciidpad (div i 1000)) ++ [chr((fromInteger(i `mod` 1000))-100)]
  
forcePrime i 
    |isPrime(i) = i 
    |otherwise = forcePrime (i+1)

createKey p q e = ((p*q),(invert p q e)) 
                    
invert p q e 
    | isJust(invertMod e ((p-1)*(q-1)))= fromJust(invertMod (e) ((p-1)*(q-1)))
    | otherwise = 1

main = do args <- getArgs
          let firstflag = (head args)
          let remArgs = drop 1 args
          case firstflag of "-gv"->do gen <- newStdGen
                                      let e = 65537
                                      let p = forcePrime(toInteger(fst (next gen)))
                                      let q = forcePrime(toInteger (fst (next (snd(next gen)))))
                                      let c = createKey p q e
                                      putStrLn("Public Exponent: "++(show e)) 
                                      putStrLn("Public Moduli: "++(show (fst c)))
                                      putStrLn("Private Exponet: "++(show (snd c)))
                            "-g" ->do gen <- newStdGen
                                      let e = 65537
                                      let p = forcePrime(toInteger(fst (next gen)))
                                      let q = forcePrime(toInteger (fst (next (snd(next gen)))))
                                      let c = createKey p q e           
                                      putStrLn (show e)
                                      putStrLn (show(fst c))
                                      putStrLn (show(snd c))
                            "-e" ->do if((length remArgs) == 3) 
                                         then do let m = head remArgs
                                                 let e = read (head(drop 1 remArgs))::Integer
                                                 let n = read (head(drop 2 remArgs))::Integer
                                                 let c = encrypt m e n  
                                                 putStrLn ("Encrypted Message: " ++(show c))
                                         else putStrLn "Incorrect Usage"
                            "-d" ->do if((length remArgs) == 3) 
                                         then do let m = read (head remArgs)::Integer
                                                 let t = read (head(drop 1 remArgs))::Integer
                                                 let n = read (head(drop 2 remArgs))::Integer
                                                 let c = decrypt m t n  
                                                 putStrLn ("Decrypted Message: " ++(show c))
                                         else putStrLn "Incorrect Usage"
                            _    ->do putStrLn "Incorrect Usage"                                 
  
    