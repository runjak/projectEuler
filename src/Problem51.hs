module Problem51 where
{--
  Task description:
  By replacing the 1st digit of the 2-digit number *3,
  it turns out that six of the nine possible values:
  13, 23, 43, 53, 73, and 83, are all prime.

  By replacing the 3rd and 4th digits of 56**3 with the same digit,
  this 5-digit number is the first example having seven primes
  among the ten generated numbers, yielding the family:
  56003, 56113, 56333, 56443, 56663, 56773, and 56993.
  Consequently 56003, being the first member of this family,
  is the smallest prime with this property.

  Find the smallest prime which,
  by replacing part of the number (not necessarily adjacent digits) with the same digit,
  is part of an eight prime value family.
--}
import Control.Arrow (second)
import Control.Monad
import Data.Char (digitToInt)
import Data.Function (on)
import Data.Monoid ((<>))
import Data.Set (Set)
import Numeric.LinearAlgebra (Z, Vector)
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Numeric.LinearAlgebra as LA

type N = Int
type Position = N
type Digit = N

familySize = 7 :: N

primes :: [N]
primes = 2 : 3 : sieve [] (tail primes) 3
  where
  notDivsBy d n     = n `rem` d /= 0
  sieve ds (p:ps) x = foldr (filter . notDivsBy) [x+2, x+4..p*p-2] ds
                   `mappend` sieve (p:ds) ps (p*p)

groupBySameLength :: [N] -> [[N]]
groupBySameLength = List.groupBy ((==) `on` (length . show))

satisfiesLength :: N -> [a] -> Bool
satisfiesLength 0 _ = True
satisfiesLength n (x:xs) = satisfiesLength (n - 1) xs
satisfiesLength n [] = False

xs :: [N]
xs = [13, 23, 43, 53, 73, 83]

ys :: [N]
ys = [56003, 56113, 56333, 56443, 56663, 56773, 56993]

{-|
Idea:
We proceed as follows:
1. Group primes from our infinite stream by common length.
2. Build a matrix out ouf the primes that share a common length.
3. For this matrix: Look at each line and consider successive lines to become links to the current line.
   A line may become a link, iff:
   * The line has the same digit at each possition as the current line
   * If the digit at a position is different:
     * The digit must be higher (because later lines will be bigger)
     * The digit must not be shared with any number of the same group
     * If any other digit of this line is also different it must be the same digit.
   * Given that we need to find links in our matrix of lines to satisfy a certain familySize
     we can also use this information to our advantage by only considering differences
     in digits that are low enough to still allow for the other options.
|-}

groupToVectors :: [N] -> [Vector Z]
groupToVectors = fmap (LA.fromList . fmap (fromIntegral . digitToInt) . show)

partitionVectors :: Vector Z -> [Vector Z] -> ([Vector Z], [Vector Z])
partitionVectors v = List.partition (linkedWith v)
  where
    linkedWith :: Vector Z -> Vector Z -> Bool
    linkedWith v w = let x = w - v
                     in check . filter (> 0) $ LA.toList x

    check :: [Z] -> Bool
    check (x:xs)
      | x > 0 = all (==x) xs
      | otherwise = False
    check [] = True

vectorPartitions :: [Vector Z] -> [[Vector Z]]
vectorPartitions [] = []
vectorPartitions (v:vs) =
  let (v1, v2) = partitionVectors v vs
  in (v:v1):vectorPartitions v2

vectorsToGroup :: [Vector Z] -> [N]
vectorsToGroup = fmap (read . (=<<) show . LA.toList)

groupPartitions :: [N] -> [[N]]
groupPartitions = fmap vectorsToGroup . vectorPartitions . groupToVectors

test = do
  pGroup <- groupBySameLength primes
  groupPartitions pGroup
