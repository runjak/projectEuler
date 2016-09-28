module Problem89(main)where
{--
  Task description:
  For a number written in Roman numerals to be considered valid there are basic rules which must be followed.
  Even though the rules allow some numbers to be expressed in more than one way there is always a "best" way of writing a particular number.

  For example, it would appear that there are at least six ways of writing the number sixteen:

  IIIIIIIIIIIIIIII
  VIIIIIIIIIII
  VVIIIIII
  XIIIIII
  VVVI
  XVI

  However, according to the rules only XIIIIII and XVI are valid,
  and the last example is considered to be the most efficient,
  as it uses the least number of numerals.

  The 11K text file, roman.txt (right click and 'Save Link/Target As...'),
  contains one thousand numbers written in valid, but not necessarily minimal, Roman numerals;
  see About... Roman Numerals for the definitive rules for this problem.

  Find the number of characters saved by writing each of these in their minimal form.

  Note: You can assume that all the Roman numerals in the file contain no more than four consecutive identical units.
--}
import Control.Monad ((<=<))
import qualified Data.List as List

file :: FilePath
file = "./p89_roman.txt"

input :: IO [String]
input = lines <$> readFile file

data RomanDigit = I | V | X | L | C | D | M
  deriving (Show, Read, Eq, Ord)

readDigits :: String -> [RomanDigit]
readDigits = fmap (read . return)

digits :: IO [[RomanDigit]]
digits = fmap (fmap readDigits) input

digitToInt :: RomanDigit -> Int
digitToInt I =    1
digitToInt V =    5
digitToInt X =   10
digitToInt L =   50
digitToInt C =  100
digitToInt D =  500
digitToInt M = 1000

romansToInt :: [RomanDigit] -> Int
romansToInt = sum . fmap go . filter (not . null) . List.tails
  where
    go :: [RomanDigit] -> Int
    go [] = 0
    go [x] = digitToInt x
    go (x:y:_)
      | x >= y = digitToInt x
      | otherwise = -(digitToInt x)

inputValues :: IO [Int]
inputValues = fmap (fmap romansToInt) digits

intToRomans :: Int -> [RomanDigit]
intToRomans x
  | x >= 1000 =     M : intToRomans (x - 1000)
  | x >=  900 = C : M : intToRomans (x -  900)
  | x >=  500 =     D : intToRomans (x -  500)
  | x >=  400 = C : D : intToRomans (x -  400)
  | x >=  100 =     C : intToRomans (x -  100)
  | x >=   90 = X : C : intToRomans (x -   90)
  | x >=   50 =     L : intToRomans (x -   50)
  | x >=   40 = X : L : intToRomans (x -   40)
  | x >=   10 =     X : intToRomans (x -   10)
  | x >=    9 = I : X : intToRomans (x -    9)
  | x >=    5 =     V : intToRomans (x -    5)
  | x >=    4 = I : V : intToRomans (x -    4)
  | x >=    1 =     I : intToRomans (x -    1)
  | otherwise = []

output = fmap (fmap (show <=< intToRomans)) inputValues

solution = do
  is <- input
  os <- output
  return . sum . fmap (\(i, o) -> length i - length o) $ zip is os

main = print =<< solution
