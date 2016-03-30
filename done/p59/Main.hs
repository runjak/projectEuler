module Main where

import Data.Function (on)
import System.IO
import Data.Bits (xor)
import Data.List (sortBy, transpose, maximumBy)
import Control.Monad (liftM)
import Data.Char (isLower, isAscii)
import qualified Data.Map as M

{- Cipher part -}
charList :: String -- | ' ' is #1 Char in most languages ô.Ô
charList = " etaoinshrdlucmwfygpbvkjzxq"

xorChar :: Char -> Char -> Char
xorChar a b =
  let
    a' = fromEnum a
    b' = fromEnum b
  in toEnum $ xor a' b'

type CMap = M.Map Char Int
getAlphabet :: String -> Alphabet
getAlphabet = mkAlphabet . mkCMap
  where
  mkCMap :: String -> CMap
  mkCMap = foldl incChar startMap

  mkAlphabet :: CMap -> Alphabet
  mkAlphabet = reverse . map fst . sortBy (compare `on` snd) . M.assocs

  startMap :: CMap
  startMap = M.empty
  
  incChar :: CMap -> Char -> CMap
  incChar = flip (M.alter inc)
    where
      inc :: Maybe Int -> Maybe Int
      inc Nothing = Just 1
      inc (Just x) = Just (x + 1)

type Alphabet = String
type Alphabets = [Alphabet]
alphabets :: [String] -> Alphabets
alphabets = map getAlphabet . transpose

type Password = String
crack :: Alphabets -> Password
crack = map (fst . maximumBy (compare `on` snd) . helper)
  where
    helper :: Alphabet -> [(Char, Int)]
    helper a = do
      c <- charList
      let f = xorChar c
      let a' = map f a
      let a'' = filter isLower a'
      return (c, length a'')

type Cipher = String
decipher :: Cipher -> Password -> String
decipher c p = zipWith xorChar c (cycle p)

asciiSum :: String -> Int
asciiSum = sum . map fromEnum

-- | Search the best fitting Char in each alphabet - eq. the one with the most lowerChar's in it's encrypted part

{- Initial functions -}
load :: String -> [Int]
load input =
  let
    s = init $ init input
    s' = '[':s++"]"
  in
    read s'

stack :: [a] -> [[a]]
stack x
  | length x >= 3 = take 3 x : stack (drop 3 x)
  | otherwise = [x]

{- All that is below shall run inside the IO Monad -}
file :: IO String
file = readFile "cipher1.txt"

loadFile :: IO [Int]
loadFile = liftM load file

loadStack :: IO [String]
loadStack = liftM (stack . map toEnum) loadFile

main :: IO ()
main = do
  stack <- loadStack
  let password = crack $ alphabets stack
  let cipher = concat stack
  let clearText = decipher cipher password
  let sum = asciiSum $ filter isAscii clearText
  putStrLn $ "Password:\t" ++ password
  putStrLn $ "Cleartext:\n" ++ clearText
  putStrLn $ "Sum:\t" ++ show sum
