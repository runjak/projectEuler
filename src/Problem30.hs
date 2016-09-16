module Problem30 where
{--
  Task description:
  Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:

      1634 = 14 + 64 + 34 + 44
      8208 = 84 + 24 + 04 + 84
      9474 = 94 + 44 + 74 + 44

  As 1 = 14 is not a sum it is not included.

  The sum of these numbers is 1634 + 8208 + 9474 = 19316.

  Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
--}

import Data.Char (digitToInt)

range = [2..]

f :: Int -> Int
f = sum . fmap ((^5) . digitToInt) . show

nums = filter (\x -> x == f x) range

main = print . sum $ take 6 nums
