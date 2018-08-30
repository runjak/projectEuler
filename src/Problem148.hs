module Problem148 where
{--
  Task description:
  We can easily verify that none of the entries in the first seven rows of Pascal's triangle are divisible by 7:
   	 	 	 	 	 	 1
   	 	 	 	 	 1	 	 1
   	 	 	 	 1	 	 2	 	 1
   	 	 	 1	 	 3	 	 3	 	 1
   	 	 1	 	 4	 	 6	 	 4	 	 1
   	 1	 	 5	 	10	 	10	 	 5	 	 1
  1	 	 6	 	15	 	20	 	15	 	 6	 	 1

  However, if we check the first one hundred rows, we will find that only 2361 of the 5050 entries are not divisible by 7.

  Find the number of entries which are not divisible by 7 in the first one billion (10^(9)) rows of Pascal's triangle.
--}

import Data.Monoid ((<>))
import Data.Ratio ((%))
import qualified Data.List as List
import qualified Data.Ratio as Ratio


type N = Integer

magic :: N
magic = 10^9

divisor :: N
divisor = 7

fac :: N -> N
fac 0 = 1
fac 1 = 1
fac n = product [1..n]

binomialCoefficient :: N -> N -> N
binomialCoefficient n k = fac n `div` (fac k * fac (n - k))

nRange :: [N]
nRange = [2 .. magic - 1]

kRangeForN :: N -> [N]
kRangeForN n = [1 .. n - 1]

parts :: [[N]]
parts = do
  n <- nRange
  return . fmap (binomialCoefficient n) $ kRangeForN n

pRows :: [[N]]
pRows = [[1],[1,1]] <> fmap (\ps -> [1] <> ps <> [1]) parts