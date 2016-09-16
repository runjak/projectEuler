module Problem17 where
{--
  Task description:
  If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

  If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

  NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
--}

import Data.Char (isLetter)

digits = ["one","two","three","four","five","six","seven","eight","nine"]
lTens = ["ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]
hTens = ["twenty-","thirty-","forty-","fifty-","sixty-","seventy-","eighty-","ninety-"]
tens = concat [digits, lTens, [ht `mappend` d | ht<-hTens, d <- "":digits]]
sHundreds = [d `mappend` " hundred" | d<-digits]
hundreds = sHundreds `mappend` [h `mappend` " and " `mappend` t | h<-sHundreds, t <- tens]
thousand = ["one thousand"]

score = length . filter isLetter
scores = fmap score

bad = concat [tens, hundreds, thousand]

main = print . sum $ scores bad
