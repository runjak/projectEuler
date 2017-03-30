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
import Control.Monad
import Data.Char (digitToInt)
import Data.Function (on)
import Data.Graph (Graph, Vertex)
import Data.Monoid ((<>))
import Numeric.LinearAlgebra (Matrix, Z, Vector)
import Data.Set (Set)
import Data.Tree (Tree, Forest)
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

linkedWith :: Vector Z -> Vector Z -> Bool
linkedWith = go Nothing `on` LA.toList
  where
    go :: Maybe (Z, Z) -> [Z] -> [Z] -> Bool
    go Nothing (v:vs) (w:ws)
      | v == w = go Nothing vs ws
      | otherwise = go (Just (v, w)) vs ws
    go vw@(Just (v', w')) (v:vs) (w:ws)
      | v == w = go vw vs ws
      | v == v' && w == w' = go vw vs ws
      | otherwise = False
    go _ [] [] = True
{- It is a pitty that `linkedWith` is only symmetric, but not transitive. -}
linkedWith' :: Vector Z -> Vector Z -> Bool
linkedWith' v w = linkedWith (min v w) (max v w)
{- Testing linkedWith' on N -}
linkedWith'' :: N -> N -> Bool
linkedWith'' = linkedWith' `on` toVector

testLinkedWith =
  let goodPairs = [(x, y) | x<-xs, y <- xs] <> [(x, y) | x<-ys, y <- ys]
      badNumbers = [10177, 21277, 32377, 54577, 65677, 76777, 87877]
      badPairs' = [(13, 79), (23, 67), (23, 89), (31, 97), (37, 59), (53, 97), (61, 83), (67, 89)]
      badPairs = badPairs' <> [(x, y) | x <- badNumbers, y <- badNumbers, x /= y]
      wanted = uncurry linkedWith''
  in and $ fmap wanted goodPairs <> fmap (not . wanted) badPairs

linkGraph :: [N] -> (Graph, Vertex -> (N, Vector Z, [Vector Z]), Vector Z -> Maybe Vertex)
linkGraph = Graph.graphFromEdges . filter (\(_,_,xs) -> not $ null xs) . computeLinks'
  where
    computeLinks :: [Vector Z] -> [(Vector Z, [Vector Z])]
    computeLinks (v:vs) =
      let vLinks = (v, [x | x <- vs, v /= x, linkedWith' v x])
      in vLinks : computeLinks vs
    computeLinks _ = []

    computeLinks' :: [N] -> [(N, Vector Z, [Vector Z])]
    computeLinks' xs = zipWith (\n (v, vs) -> (n, v, vs)) xs . computeLinks $ fmap toVector xs

linkedGraphs :: [(Graph, Vertex -> N)]
linkedGraphs = let lookup f v = (\(n, _, _) -> n) $ f v
                   wrap (g, lookup', _) = (g, lookup lookup')
               in wrap . linkGraph <$> groupBySameLength primes

drawForest :: (a -> String) -> Forest a -> IO ()
drawForest toString = putStrLn . Tree.drawForest . fmap (fmap toString)

drawTree :: (a -> String) -> Tree a -> IO ()
drawTree toString = putStrLn . Tree.drawTree . fmap toString

-- It appears that finding the desired numbers maps neatly onto the clique problem.
findNextFor :: Graph -> Set Vertex -> Set Vertex
findNextFor g vSet =
  let canReach = (Set.fromList . (Arr.!) g) <$> Set.toList vSet
      wanted = Set.difference `flip` vSet
  in reduce $ fmap wanted canReach
  where
    reduce :: [Set Vertex] -> Set Vertex
    reduce [] = Set.empty
    reduce rs = foldl1 Set.intersection rs

{-
https://www.andres-loeh.de/IFIP-MCE.pdf
https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm
-}
bronKerbosch :: (Set Vertex -> Set Vertex) -> Set Vertex -> Set Vertex -> Set Vertex -> [Set Vertex]
bronKerbosch findNext compsub cand excl
  | Set.null cand && Set.null excl = [compsub]
  | otherwise = concat $ List.unfoldr go (Set.toList cand, cand, excl)
    where
      go :: ([Vertex], Set Vertex, Set Vertex) -> Maybe ([Set Vertex], ([Vertex], Set Vertex, Set Vertex))
      go ([], _, _) = Nothing
      go (v : vs, cand, excl) =
        let vSet = Set.singleton v
            nextSet = findNext vSet
            compsub' = Set.union compsub vSet
            cand' = Set.intersection cand nextSet
            excl' = Set.intersection excl nextSet
            nextState = (vs, Set.difference cand vSet, Set.union excl vSet)
        in Just (bronKerbosch findNext compsub' cand' excl', nextState)
-- Let's use bronKerbosch on the Graphs we have!
bronKerbosch' :: (Graph, Vertex -> N) -> [Set N]
bronKerbosch' (g, lookup) =
  let findNext = findNextFor g
      cand = Set.fromList $ Graph.vertices g
  in Set.map lookup <$> bronKerbosch findNext Set.empty cand Set.empty

cliques :: [[Set N]]
cliques = fmap (simplify . List.sort . bronKerbosch') linkedGraphs
  where
    simplify :: [Set N] -> [Set N]
    simplify (a:b:cs)
      | Set.isSubsetOf b a = simplify (a : cs)
      | otherwise = a : simplify (b : cs)
    simplify x = x

wantedBySize :: N -> [Set N]
wantedBySize size = filter ((>= size) . Set.size) $ concat cliques

testWantedBySize :: Bool
testWantedBySize =
  let search = Set.toList . head . wantedBySize . length
      xs' = search xs
      ys' = search ys
  in xs == xs' && ys == ys'
