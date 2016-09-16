module Problem22 where
{--
  Task description:
  Using names.txt (right click and 'Save Link/Target As...'),
  a 46K text file containing over five-thousand first names,
  begin by sorting it into alphabetical order.
  Then working out the alphabetical value for each name,
  multiply this value by its alphabetical position in the list to obtain a name score.

  For example, when the list is sorted into alphabetical order,
  COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53,
  is the 938th name in the list.
  So, COLIN would obtain a score of 938 × 53 = 49714.

  What is the total of all the name scores in the file?
--}

import System.IO (readFile)
import Control.Monad (liftM)
import Data.List (sort)

file = readFile "p22_names.txt"

mkLines :: String -> String
mkLines (x:xs)
  | x == '\"' = mkLines xs
  | x == ',' = '\n':mkLines xs
  | otherwise = x : mkLines xs
mkLines [] = []

sortedLines :: String -> [String]
sortedLines = sort . lines . mkLines

alphaScore :: String -> Integer
alphaScore = sum . fmap (flip (-) 64 . toInteger . fromEnum)

scores :: String -> [Integer]
scores = zipWith (*) [1..] . fmap alphaScore . sortedLines

main :: IO ()
main = print =<< fmap (sum . scores) file
