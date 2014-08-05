module Main where

import Data.Char (isLetter)

digits = ["one","two","three","four","five","six","seven","eight","nine"]
lTens = ["ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]
hTens = ["twenty-","thirty-","forty-","fifty-","sixty-","seventy-","eighty-","ninety-"]
tens = digits ++ lTens ++ [ht++d|ht<-hTens, d <- ("":digits)]
sHundreds = [d++" hundred"|d<-digits]
hundreds = sHundreds ++ [h++" and "++t|h<-sHundreds, t <- tens]
thousand = ["one thousand"]

score = length . filter isLetter
scores = map score

bad = tens ++ hundreds ++ thousand

main = print . sum $ scores bad
