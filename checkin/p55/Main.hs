module Main where

import Control.Arrow (first)
import Data.Set (Set)
import qualified Data.Set as Set

type N = Integer

end = 10000 :: N

range :: [N]
range = [1..end]

mirror :: N -> N
mirror = read . reverse . show

step :: N -> N
step n = mirror n + n

steps :: N -> [N]
steps = iterate step

palindrome :: N -> Bool
palindrome n = let n' = show n
               in n' == reverse n'

test :: [N] -> ([N], Bool)
test (n:ns)
  | palindrome (head ns) = ([n], True)
  | n <= end             = first (n:) $ test ns
  | otherwise            = ([], any palindrome $ take 50 ns)

tests :: (Set N, Set N) -> N -> (Set N, Set N)
tests x@(good, bad) n
  | Set.member n good = x
  | Set.member n bad  = x
  | otherwise       = let (ns, pass) = test $ steps n
                          f s        = Set.union s $ Set.fromList ns
                          good'      = f good
                          bad'       = f bad
                      in if pass then (good', bad) else (good, bad')

main = print . Set.size . snd $ foldl tests (Set.empty, Set.empty) range
