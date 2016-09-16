module Problem20 where
{--
  Task description:
  n! means n × (n − 1) × ... × 3 × 2 × 1

  Find the sum of the digits in the number 100!
--}

import Data.Char (digitToInt)

fac :: Integer -> Integer
fac 1 = 1
fac x = fac (x-1) * x

magicNumber :: Integer
magicNumber = fac 100

digitSum :: Int
digitSum = sum . fmap digitToInt $ show magicNumber

main = print digitSum
