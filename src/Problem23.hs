module Problem23 where
{--
  Task description:
  A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

  A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.

  As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.

  Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
--}

import Control.Monad (guard)
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

type N = Int

upperLimit :: N
upperLimit = 28123

range = [1..upperLimit]

root :: N -> N
root = round . sqrt . fromIntegral

divisors :: N -> [N]
divisors x = nub $ 1 : do
  p <- [2..(root x)]
  guard $ (x`mod`p) == 0
  [p,x`div`p]

abundant :: N -> Bool
abundant x = (x <) . sum $ divisors x

type Abundant  = N
type Nabundant = N
ans :: ([Abundant],[Nabundant])
ans = partition abundant range

abundants  = fst ans
nabundants = snd ans

{-|
  aSums holds all numbers below upperLimit
  that are the sum of two abundants.
|-}
aSums :: Set N
aSums = Set.fromList $ do
  x <- abundants
  y <- abundants
  let s = x+y
  guard $ s <= upperLimit
  return s

{-|
  nSums holds all numbers below upperLimit
  that are not found in aSums.
|-}
nSums :: [N]
nSums = filter (not . flip Set.member aSums) range

main = print $ sum nSums
