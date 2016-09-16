module Problem79 where
{--
  Task description:
  A common security method used for online banking is to ask the user for three random characters from a passcode.
  For example, if the passcode was 531278, they may ask for the 2nd, 3rd, and 5th characters; the expected reply would be: 317.

  The text file, keylog.txt, contains fifty successful login attempts.

  Given that the three characters are always asked for in order, analyse the file so as to determine the shortest possible secret passcode of unknown length.
--}

import System.IO (readFile)
import Data.Char (isDigit)
import Data.List (nub)

input :: IO [(Char,Char,Char)]
input = do
  rawData <- readFile "p79_keylog.txt"
  let triplets = mkTriplets $ filter isDigit rawData
  return $ nub triplets

mkTriplets :: String -> [(Char,Char,Char)]
mkTriplets (a:b:c:xs) = (a,b,c):mkTriplets xs
mkTriplets _ = []

setFirst :: Char -> Char -> String -> String
setFirst a b (x:xs)
  | x == a = x:xs
  | x == b = a:replace a b xs
  | otherwise = x : setFirst a b xs
  where
    replace :: Char -> Char -> String -> String
    replace a b (x:xs)
      | x == a = b : xs
      | otherwise = x : replace a b xs

imposeTriplet :: String -> (Char,Char,Char) -> String
imposeTriplet list (a,b,c) = setFirst a b . setFirst a c $ setFirst b c list

main :: IO ()
main = do
  triplets <- input
  let code = foldl imposeTriplet "01236789" triplets
  print code
