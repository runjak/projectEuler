module Problem24 where
{--
  Task description:
  A permutation is an ordered arrangement of objects.
  For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4.
  If all of the permutations are listed numerically or alphabetically, we call it lexicographic order.
  The lexicographic permutations of 0, 1 and 2 are:

  012   021   102   120   201   210

  What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
--}

import Data.List

magic :: Int
magic = 10^6

mList :: [Int]
mList = [0..9]

type Fact = Int
facts :: [Fact]
facts = 1 : go 1 [1..]
  where
    go :: Fact -> [Fact] -> [Fact]
    go x (y:ys) = let z = x*y in z:go z ys

crunch :: Int -> [Maybe Int]
crunch x = crunch' (init . reverse $ takeWhile (<=x) facts) x
  where
    crunch' :: [Fact] -> Int -> [Maybe Int]
    crunch' [] _ = []
    crunch' (f:fs) x
      | x < f = Nothing : crunch' fs x
      | otherwise =
        let
          i = x `div` f
          x' = x - f * i
        in Just i : crunch' fs x'

fetch :: Int -> [a] -> (a,[a])
fetch _ [] = error "fetch from empty"
fetch x l
  | x < 0 = error "fetch negative"
  | otherwise = fetch' [] x l
    where
      fetch' _ _ [] = error "fetch out of list"
      fetch' ks 0 (l:ls) = (l, reverse ks `mappend` ls)
      fetch' ks x (l:ls) = fetch' (l : ks) (x - 1) ls

apply :: [a] -> [Maybe Int] -> [a]
apply [] _ = []
apply l [] = l
apply (a:as) (Nothing:ls) = a : apply as ls
apply a' (Just i:ls) =
  let (a, as) = fetch i a'
  in a : apply as ls

permute :: [a] -> Int -> [a]
permute [] _ = error "permute from empty"
permute a x
  | x == 0 = a
  | x < 0 = error "permute from negative"
  | otherwise = apply a $ crunch x

main :: IO ()
main = print . concatMap show . permute mList $ magic - 1
