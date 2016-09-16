module Problem49 where
{--
  Task description:
  The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330,
  is unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.

  There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
  exhibiting this property, but there is one other 4-digit increasing sequence.

  What 12-digit number do you form by concatenating the three terms in this sequence?
--}

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

primes :: [Int]
primes = 2: 3: sieve [] (tail primes) 3
  where
  notDivsBy d n     = n `rem` d /= 0
  sieve ds (p:ps) x = foldr (filter . notDivsBy) [x+2,x+4..p*p-2] ds
                    `mappend` sieve (p:ds) ps (p*p)

wanted = takeWhile (<10000) $ dropWhile (<1000) primes
wantedSet = Set.fromList wanted

mkTriple :: Int -> Int -> (Int, Int, Int)
mkTriple x y = (x, y, 2*y-x)

lastInSet :: (Int,Int,Int) -> Bool
lastInSet (_, _, z) = Set.member z wantedSet

triples :: [(Int,Int,Int)]
triples = filter lastInSet . (go =<<) $ tails wanted
  where
    go :: [Int] -> [(Int, Int, Int)]
    go (x:xs) = fmap (mkTriple x) xs
    go _      = []

arePerms :: (Int, Int, Int) -> Bool
arePerms (x, y, z) = let x' = sort $ show x
                         y' = sort $ show y
                         z' = sort $ show z
                     in x' == y' && x' == z'

notKnown = (/= (1487, 4817, 8147))

main = mapM_ (putStrLn . (\(x,y,z) -> concat [show x, show y, show z])) . filter notKnown $ filter arePerms triples
