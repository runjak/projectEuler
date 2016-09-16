module Problem33 where
{--
  Task description:
  The fraction 49/98 is a curious fraction,
  as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8,
  which is correct, is obtained by cancelling the 9s.

  We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

  There are exactly four non-trivial examples of this type of fraction, less than one in value,
  and containing two digits in the numerator and denominator.

  If the product of these four fractions is given in its lowest common terms,
  find the value of the denominator.
--}

import Control.Monad
import Data.Ratio ((%))
import qualified Data.Ratio as Ratio

type N = Int

ratios :: [(N, N)]
ratios = [(y,x)| x <- xs, y <- takeWhile (< x) xs]
  where
    xs = [x | x <- [11..99], x `mod` 10 /= 0]

same :: (N, N) -> (N, N) -> Bool
same (a,b) (c,d) = a%b == c%d

predicate :: (N, N) -> Bool
predicate ratio@(x, y) = or $ do
  let x' = show x
      y' = show y
  d <- show x
  guard $ d `elem` y'
  let y'' = go d y'
      x'' = go d x'
  guard $ same ratio (read x'', read y'')
  return True
  where
    go :: Char -> String -> String
    go c (x:xs)
      | c == x = xs
      | otherwise = x : go c xs
    go c [] = []

wanted = filter predicate ratios

solution = Ratio.denominator . product $ fmap (uncurry (%)) wanted

main = print solution
