module Problem38 where
{--
  Task description:
  Take the number 192 and multiply it by each of 1, 2, and 3:

  192 × 1 = 192
  192 × 2 = 384
  192 × 3 = 576
  By concatenating each product we get the 1 to 9 pandigital, 192384576.
  We will call 192384576 the concatenated product of 192 and (1,2,3)

  The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5,
  giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).

  What is the largest 1 to 9 pandigital 9-digit number that can be formed as
  the concatenated product of an integer with (1,2, ... , n) where n > 1?
--}

import Data.List

type N = Int

digits :: String
digits = "123456789"

searchSpace :: [N]
searchSpace = [1..9999]

pandigital :: N -> Bool
pandigital = (== digits) . sort . show

cProds :: N -> [N]
cProds n = let ns = tail . inits $ fmap (*n) [1..9]
           in fmap (read . (show =<<)) ns

cProdSpace = cProds =<< searchSpace

solution = maximum $ filter pandigital cProdSpace

main = print solution
