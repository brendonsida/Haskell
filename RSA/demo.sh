#!/bin/bash 
echo "Generate RSA key"
echo "RSA -g"
lines=$(RSA -g)
IFS=' ' read e n t <<< $lines  
echo "Public Exponent: $e"
echo "Public Modulus: $n"
echo "Private Exponent: $t"
echo ""

echo "Encrypting Message: Hello"
echo "RSA -e Hello $e $n"
encrypted=$(RSA -e Hello "$e" "$n")
echo $encrypted
IFS=' ' read junk1 junk2 m <<< $encrypted 

echo ""
echo "Decrypting Message: $m"
echo "RSA -d $m $t $n" 
RSA -d "$m" "$t" "$n"
