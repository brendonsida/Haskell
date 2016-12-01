import Data.Char (ord)
import Data.Char (chr)
import System.Random
import Math.NumberTheory.Primes (isPrime)
import Math.NumberTheory.Moduli (invertMod)
import Math.NumberTheory.Moduli (powerModInteger)
import Data.Maybe
import Debug.Trace
import System.Environment (getArgs)

--Encrypt String to ciphertext 
encrypt m e n = powerModInteger (asciipad m) e n
--Decrpyt ciphertext to String
decrypt m t n = asciidpad(powerModInteger m t n)

--Convert String to an Integer equal to the concat of its padded ascii charaters
asciipad s
  |((length s) == 0) =  0 
  |otherwise = ((100 + toInteger((ord (head s)))) *(toInteger(1000 ^((length s)-1))))+(toInteger(asciipad (drop 1 s)))

--Convert padded ascii characters  to string 
asciidpad i
  | i <= 100 = ""
  | otherwise =(asciidpad (div i 1000)) ++ [chr((fromInteger(i `mod` 1000))-100)]

--find next prime after value
forcePrime i 
    |isPrime(i) = i 
    |otherwise = forcePrime (i+1)
    
--Create RSA key from two primes and exponent
createKey p q e = ((p*q),(invert p q e)) 

--Wrapper function for invertMod to handle invalid cases
invert p q e 
    | isJust(invertMod e ((p-1)*(q-1)))= fromJust(invertMod (e) ((p-1)*(q-1)))
    | otherwise = 1

main = do args <- getArgs
          let firstflag = (head args)
          let remArgs = drop 1 args
                             --generate random key values with labels
          case firstflag of "-gv"->do gen <- newStdGen
                                      let e = 65537
                                      let p = forcePrime(toInteger(fst (next gen)))
                                      let q = forcePrime(toInteger (fst (next (snd(next gen)))))
                                      let c = createKey p q e
                                      putStrLn("Public Exponent: "++(show e)) 
                                      putStrLn("Public Moduli: "++(show (fst c)))
                                      putStrLn("Private Exponet: "++(show (snd c)))
                            --Generate random key values
                            "-g" ->do gen <- newStdGen
                                      let e = 65537
                                      let p = forcePrime(toInteger(fst (next gen)))
                                      let q = forcePrime(toInteger (fst (next (snd(next gen)))))
                                      let c = createKey p q e           
                                      putStrLn (show e)
                                      putStrLn (show(fst c))
                                      putStrLn (show(snd c))
                            --Encrypt provided text using provided public key          
                            "-e" ->do if((length remArgs) == 3) 
                                         then do let m = head remArgs
                                                 let e = read (head(drop 1 remArgs))::Integer
                                                 let n = read (head(drop 2 remArgs))::Integer
                                                 let c = encrypt m e n  
                                                 putStrLn ("Encrypted Message: " ++(show c))
                                         else putStrLn "Incorrect Usage"]
                            --Decrypt provided encrypted text using provided private key             
                            "-d" ->do if((length remArgs) == 3) 
                                         then do let m = read (head remArgs)::Integer
                                                 let t = read (head(drop 1 remArgs))::Integer
                                                 let n = read (head(drop 2 remArgs))::Integer
                                                 let c = decrypt m t n  
                                                 putStrLn ("Decrypted Message: " ++(show c))
                                         else putStrLn "Incorrect Usage"
                            --Default Case             
                            _    ->do putStrLn "Incorrect Usage"                                 
  
    
