module Problem44 where
{--
  Task description:
  Pentagonal numbers are generated by the formula, Pn=n(3n−1)/2.

  The first ten pentagonal numbers are:
  1, 5, 12, 22, 35, 51, 70, 92, 117, 145, …

  It can be seen that P4 + P7 = 22 + 70 = 92 = P8.
  However, their difference, 70 − 22 = 48, is not pentagonal.

  Find the pair of pentagonal numbers, Pj and Pk,
  for which their sum and difference are pentagonal and D = |Pk − Pj| is minimised;
  what is the value of D?
--}

import Control.Monad
import qualified Data.Maybe as Maybe

type N = Integer

pentagonal :: N -> N
pentagonal n = n*(3*n - 1) `div` 2

unPentagonal :: N -> Maybe N
unPentagonal n = let y = 2 * fromIntegral n
                     x = (sqrt (12*y + 1) + 1)/6
                 in if ceiling x == floor x
                    then Just $ floor x
                    else Nothing

isPentagonal :: N -> Bool
isPentagonal = Maybe.isJust . unPentagonal

pentagonals :: [N]
pentagonals = fmap pentagonal [1..]

pPairs :: [(N, N)]
pPairs = do
  (n, y) <- zip [1..] pentagonals
  let next  = pentagonal $ n + 1
      upper = takeWhile (< y)
      lower = dropWhile (>= (next - y))
  x <- upper . lower $ pentagonals
  return (x, y)

diffs :: [(N, N, N)]
diffs = do
  (x,y) <- pPairs
  guard . isPentagonal $ x + y
  let d = y - x
  guard $ isPentagonal d
  return (d,x,y)

solution = let (s, _, _) = head diffs
           in s

main = print solution