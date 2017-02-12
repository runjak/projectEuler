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
import Data.Tree (Tree, Forest)
import Numeric.LinearAlgebra (Z, Vector)
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Tree as Tree

type N = Int
type Position = N
type Digit = N

familySize = 8 :: N

primes :: [N]
primes = 2 : 3 : sieve [] (tail primes) 3
  where
  notDivsBy d n     = n `rem` d /= 0
  sieve ds (p:ps) x = foldr (filter . notDivsBy) [x+2, x+4..p*p-2] ds
                   `mappend` sieve (p:ds) ps (p*p)

groupBySameLength :: [N] -> [[N]]
groupBySameLength = List.groupBy ((==) `on` (length . show))

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

toVector :: N -> Vector Z
toVector = LA.fromList . fmap (fromIntegral . digitToInt) . show

fromVector :: Vector Z -> N
fromVector = read . (=<<) show . LA.toList

groupToVectors :: [N] -> [Vector Z]
groupToVectors = fmap toVector

vectorsToGroup :: [Vector Z] -> [N]
vectorsToGroup = fmap fromVector

partitionVectors :: Vector Z -> [Vector Z] -> ([Vector Z], [Vector Z])
partitionVectors v = List.partition (linkedWith v)

{- It is a pitty that `linkedWith` is only symmetric, but not transitive. -}
linkedWith :: Vector Z -> Vector Z -> Bool
linkedWith v w =
  let x = (max v w) - (min v w)
  in go 0 $ LA.toList x
  where
    go 0 (x:xs)
      | x < 0 = False
      | x > 0 = go x xs
      | otherwise = go 0 xs
    go x (y:ys)
      | y == 0 = go x ys
      | x == y = go x ys
      | otherwise = False
    go _ [] =  True

vectorPartitions :: [Vector Z] -> [[Vector Z]]
vectorPartitions [] = []
vectorPartitions (v:vs) =
  let (v1, v2) = partitionVectors v vs
  in (v:v1):vectorPartitions v2

groupPartitions :: [N] -> [[N]]
groupPartitions = fmap vectorsToGroup . vectorPartitions . groupToVectors

test = do
  pGroup <- groupBySameLength primes
  groupPartitions pGroup

t = test !! 1

toTree' :: [N] -> Tree N
toTree' = fmap fromVector . toTree . fmap toVector

toTree :: [Vector Z] -> Tree (Vector Z)
toTree (p:ps) = foldl insertInTree (Tree.Node p []) ps
  where
    insertInTree :: Tree (Vector Z) -> Vector Z -> Tree (Vector Z)
    insertInTree t p = t{Tree.subForest = insertInForest (Tree.subForest t) p}

    insertInForest :: Forest (Vector Z) -> Vector Z -> Forest (Vector Z)
    insertInForest [] p = [Tree.Node p []]
    insertInForest (t:ts) p
      | linkedWith (Tree.rootLabel t) p = (insertInTree t p):ts
      | otherwise = t:insertInForest ts p

test' = toTree' <$> groupBySameLength primes

test'' = fmap (Tree.drawTree . fmap show) test'

satisfiesLength :: N -> [a] -> Bool
satisfiesLength 0 _ = True
satisfiesLength n (x:xs) = satisfiesLength (n - 1) xs
satisfiesLength n [] = False
