module Problem14 where
{--
  Task description:
  The following iterative sequence is defined for the set of positive integers:

  n → n/2 (n is even)
  n → 3n + 1 (n is odd)

  Using the rule above and starting with 13, we generate the following sequence:
  13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

  It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.
  Although it has not been proved yet (Collatz Problem),
  it is thought that all starting numbers finish at 1.

  Which starting number, under one million, produces the longest chain?

  NOTE: Once the chain starts the terms are allowed to go above one million.
--}

import Data.Function (on)
import Data.List (maximumBy)

border :: Int
border = 999999

range = [1..border]

collatz :: (Integral a) => a -> a
collatz n
  | odd n = 3*n+1
  | otherwise = n`div`2

chain :: Int -> [Int]
chain 1 = [1]
chain x = let y = collatz x in y : chain y

score = length . chain

vals = [(score x, x)|x<-range]

main = print $ maximumBy (compare `on` fst) vals
