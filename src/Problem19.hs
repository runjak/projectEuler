module Problem19 where
{--
  Task description:
  You are given the following information, but you may prefer to do some research for yourself.

      1 Jan 1900 was a Monday.
      Thirty days has September,
      April, June and November.
      All the rest have thirty-one,
      Saving February alone,
      Which has twenty-eight, rain or shine.
      And on leap years, twenty-nine.
      A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.

  How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
--}

import Data.List (elem)

type Year = Int

years :: [Year]
years = [1901..2000]

isLeap :: Year -> Bool
isLeap year =
  (year `mod` 4 == 0) &&
    (((year `mod` 100) /= 0) || (year `mod` 400 == 0))

data Month = January | February | March | April | May | June | July | August | September | October | November | December
  deriving(Eq, Enum, Show)
months = [January ..]

daysInMonth :: Year -> Month -> Int
daysInMonth year month
  | month == February = if isLeap year then 29 else 28
  | month `elem` [January, March, May, July, August, October, December] = 31
  | otherwise = 30

data Day = Fill | Sunday deriving (Eq, Show)
days = cycle $ replicate 6 Fill `mappend` [Sunday]

daysAfter1900 = flip drop days . sum $ fmap (daysInMonth 1900) months

allMonths = concat . zipWith (\y ms -> fmap (daysInMonth y) ms) years $ repeat months

go :: ([Day],Int) -> Int -> ([Day],Int)
go (d, score) month
  | head d == Sunday = (drop month d, score + 1)
  | otherwise = (drop month d, score)

main = print . snd $ foldl go (daysAfter1900, 0) allMonths
