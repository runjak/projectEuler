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
import Data.Function (on)
import Data.Set (Set)
import qualified Data.List as List
import qualified Data.Set as Set

type N = Int

primes :: [N]
primes = 2 : 3 : sieve [] (tail primes) 3
  where
  notDivsBy d n     = n `rem` d /= 0
  sieve ds (p:ps) x = foldr (filter . notDivsBy) [x+2, x+4..p*p-2] ds
                   `mappend` sieve (p:ds) ps (p*p)

pGroups :: [Set N]
pGroups = Set.fromList <$> List.groupBy ((==) `on` (length . show)) primes
