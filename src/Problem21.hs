module Problem21 where
{--
  Task description:
  Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
  If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.

  For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

  Evaluate the sum of all the amicable numbers under 10000.
--}

import Control.Monad (guard)
import Data.List (nub, zip3)

range :: [Int]
range = [1..9999]

intSqr :: Int -> Int
intSqr = floor . sqrt . fromIntegral

divisors :: Int -> [Int]
divisors x = (1:) . nub . concat $ do
  a <- [2..intSqr x]
  guard (x`rem`a == 0)
  return [a, x`div`a]

divSum :: Int -> Int
divSum = sum . divisors

amicables = fmap (\(x,_,_)->x) . filter (\(a,b,c)->a==c && a/=b) . (\(x:y:z:_)->zip3 x y z) . take 3 . iterate (fmap divSum)

main = print . sum $ amicables range
