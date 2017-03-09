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
import Control.Arrow (second, (***))
import Control.Monad
import Data.Char (digitToInt)
import Data.Function (on)
import Data.Graph (Graph, Vertex)
import Data.Monoid ((<>))
import Numeric.LinearAlgebra (Matrix)
import Data.Set (Set)
import Data.Tree (Tree, Forest)
import Numeric.LinearAlgebra (Z, Vector)
import qualified GHC.Arr as Arr
import qualified Data.Graph as Graph
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Tree as Tree
import qualified Numeric.LinearAlgebra as LA

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

{- It is a pitty that `linkedWith'` is only symmetric, but not transitive. -}
linkedWith' v w = linkedWith (min v w) (max v w)
linkedWith :: Vector Z -> Vector Z -> Bool
linkedWith v w =
  let x = w - v
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

linkGraph :: [N] -> (Graph, Vertex -> (N, Vector Z, [Vector Z]), Vector Z -> Maybe Vertex)
linkGraph = Graph.graphFromEdges . filter (\(_,_,xs) -> not $ null xs) . computeLinks'
  where
    computeLinks :: [Vector Z] -> [(Vector Z, [Vector Z])]
    computeLinks [] = []
    computeLinks vs = do
      v <- vs
      return (v, [x | x <- vs, v /= x, linkedWith' v x] )

    computeLinks' :: [N] -> [(N, Vector Z, [Vector Z])]
    computeLinks' xs = zipWith (\n (v, vs) -> (n, v, vs)) xs . computeLinks $ fmap toVector xs

graphToMatrix :: Graph -> Matrix Double
graphToMatrix = LA.toDense . graphToAssocMatrix
  where
    graphToAssocMatrix :: Graph -> LA.AssocMatrix
    graphToAssocMatrix g = do
      (v, targets) <- Arr.assocs g
      [((v, t), 1)| t <- targets]

mPow :: Matrix Double -> Int -> Matrix Double
mPow m 0 = LA.ident $ LA.rows m
mPow m 1 = m
mPow m n
  | even n = mPow (m <> m) (n `div` 2)
  | otherwise = m <> mPow m (n - 1)

test = groupBySameLength primes
test' = fmap linkGraph test
test'' = fmap (\(g,_,_) -> graphToMatrix g) test'
