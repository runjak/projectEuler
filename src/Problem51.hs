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
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Monoid ((<>))
import qualified Data.List as List
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap

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

toVector :: N -> [N]
toVector = fmap digitToInt . show

linkedWith :: [N] -> [N] -> Bool
linkedWith = go Nothing
  where
    go :: Maybe (N, N) -> [N] -> [N] -> Bool
    go Nothing (v:vs) (w:ws)
      | v == w = go Nothing vs ws
      | otherwise = go (Just (v, w)) vs ws
    go vw@(Just (v', w')) (v:vs) (w:ws)
      | v == w = go vw vs ws
      | v == v' && w == w' = go vw vs ws
      | otherwise = False
    go _ [] [] = True
{- It is a pitty that `linkedWith` is only symmetric, but not transitive. -}
linkedWith' :: N -> N -> Bool
linkedWith' a b =
  let v = toVector $ min a b
      w = toVector $ max a b
  in linkedWith v w

testLinkedWith :: Bool
testLinkedWith =
  let goodPairs = [(x, y) | x<-xs, y <- xs] <> [(x, y) | x<-ys, y <- ys]
      badNumbers = [10177, 21277, 32377, 54577, 65677, 76777, 87877]
      badPairs' = [(13, 79), (23, 67), (23, 89), (31, 97), (37, 59), (53, 97), (61, 83), (67, 89)]
      badPairs = badPairs' <> [(x, y) | x <- badNumbers, y <- badNumbers, x /= y]
      wanted = uncurry linkedWith'
  in and $ fmap wanted goodPairs <> fmap (not . wanted) badPairs

linkMap :: [N] -> HashMap N (HashSet N)
linkMap = HashMap.fromList . computeLinks
  where
    computeLinks :: [N] -> [(N, HashSet N)]
    computeLinks ns = do
      n <- ns
      let linked = [m | m <- ns, m /= n, linkedWith' n m]
      return (n, HashSet.fromList linked)

linkedMaps :: [HashMap N (HashSet N)]
linkedMaps = linkMap <$> groupBySameLength primes

{-
https://www.andres-loeh.de/IFIP-MCE.pdf
https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm
https://hackage.haskell.org/package/maximal-cliques-0.1.1/docs/src/Data-Algorithm-MaximalCliques.html#maximalCliques
-}
bronKerbosch :: (N -> HashSet N) -> HashSet N -> HashSet N -> HashSet N -> [HashSet N]
bronKerbosch findNext compsub cand excl
  | HashSet.null cand && HashSet.null excl = [compsub]
  | otherwise = concat $ List.unfoldr go (HashSet.toList cand, cand, excl)
    where
      go :: ([N], HashSet N, HashSet N) -> Maybe ([HashSet N], ([N], HashSet N, HashSet N))
      go ([], _, _) = Nothing
      go (v : vs, cand, excl) =
        let nextSet = findNext v
            compsub' = HashSet.insert v compsub
            cand' = HashSet.intersection cand nextSet
            excl' = HashSet.intersection excl nextSet
            nextState = (vs, HashSet.delete v cand, HashSet.insert v excl)
        in Just (bronKerbosch findNext compsub' cand' excl', nextState)
-- Let's use bronKerbosch on the Graphs we have!
bronKerbosch' :: HashMap N (HashSet N) -> [HashSet N]
bronKerbosch' g =
  let findNext = (HashMap.!) g
      cand = HashSet.fromList $ HashMap.keys g
  in bronKerbosch findNext HashSet.empty cand HashSet.empty

-- | The Bron-Kerbosch algorithm for finding all maximal cliques in an undirected graph.
-- <http://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm>. Works on nodes represented as 'Int's.
maximalCliques :: (HashSet N -> HashSet N -> N) -- ^ A function that given two 'IntSet's, chooses a member of one as a pivot.
               -> (N -> HashSet N)  -- ^ A function that given a node id, yields the set of its neighbors.
               -> HashSet N -- ^ The set of all nodes in the graph.
               -> [HashSet N] -- ^ An enumeration of all maximal cliques in the graph.
maximalCliques pickpivot neighborsOf nodeset = go HashSet.empty nodeset HashSet.empty
    where go r p x
              | HashSet.null p && HashSet.null x = [r]
              | otherwise =
                  let pivot = pickpivot p x
                      step' (p',x') v =
                          let nv  = neighborsOf v
                          in ((HashSet.delete v p', HashSet.insert v x'), go (HashSet.insert v r) (HashSet.intersection nv p') (HashSet.intersection nv x'))
                  in concat . snd . List.mapAccumL step' (p,x) $ HashSet.toList (p `HashSet.difference` neighborsOf pivot)

cliques :: [[HashSet N]]
cliques = fmap bronKerbosch' linkedMaps

wantedBySize :: N -> [HashSet N]
wantedBySize size = filter ((>= size) . HashSet.size) $ concat cliques

testWantedBySize :: Bool
testWantedBySize =
  let search = head . wantedBySize . length
      test wanted = (HashSet.fromList wanted) == (search wanted)
  in all test [xs, ys]

problem51 :: N
problem51 = minimum . head $ wantedBySize familySize

main = print problem51
