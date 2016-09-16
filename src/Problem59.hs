module Problem59 where
{--
  Task description:
  Each character on a computer is assigned a unique code and the preferred standard is ASCII (American Standard Code for Information Interchange).
  For example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.
  A modern encryption method is to take a text file, convert the bytes to ASCII,
  then XOR each byte with a given value, taken from a secret key.
  The advantage with the XOR function is that using the same encryption key on the cipher text,
  restores the plain text; for example, 65 XOR 42 = 107, then 107 XOR 42 = 65.
  For unbreakable encryption, the key is the same length as the plain text message,
  and the key is made up of random bytes.
  The user would keep the encrypted message and the encryption key in different locations,
  and without both "halves", it is impossible to decrypt the message.
  Unfortunately, this method is impractical for most users, so the modified method is to use a password as a key.
  If the password is shorter than the message, which is likely, the key is repeated cyclically throughout the message.
  The balance for this method is using a sufficiently long password key for security, but short enough to be memorable.
  Your task has been made easy, as the encryption key consists of three lower case characters.
  Using cipher1.txt (right click and 'Save Link/Target As...'), a file containing the encrypted ASCII codes,
  and the knowledge that the plain text must contain common English words, decrypt the message and find the sum of the ASCII values in the original text.
--}

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
  mkAlphabet = reverse . fmap fst . sortBy (compare `on` snd) . M.assocs

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
alphabets = fmap getAlphabet . transpose

type Password = String
crack :: Alphabets -> Password
crack = fmap (fst . maximumBy (compare `on` snd) . helper)
  where
    helper :: Alphabet -> [(Char, Int)]
    helper a = do
      c <- charList
      let f = xorChar c
      let a' = fmap f a
      let a'' = filter isLower a'
      return (c, length a'')

type Cipher = String
decipher :: Cipher -> Password -> String
decipher c p = zipWith xorChar c (cycle p)

asciiSum :: String -> Int
asciiSum = sum . fmap fromEnum

-- | Search the best fitting Char in each alphabet - eq. the one with the most lowerChar's in it's encrypted part

{- Initial functions -}
load :: String -> [Int]
load input =
  let
    s = init $ init input
    s' = '[':s `mappend` "]"
  in
    read s'

stack :: [a] -> [[a]]
stack x
  | length x >= 3 = take 3 x : stack (drop 3 x)
  | otherwise = [x]

{- All that is below shall run inside the IO Monad -}
file :: IO String
file = readFile "p59_cipher1.txt"

loadFile :: IO [Int]
loadFile = fmap load file

loadStack :: IO [String]
loadStack = fmap (stack . fmap toEnum) loadFile

main :: IO ()
main = do
  stack <- loadStack
  let password = crack $ alphabets stack
  let cipher = concat stack
  let clearText = decipher cipher password
  let sum = asciiSum $ filter isAscii clearText
  putStrLn $ "Password:\t" `mappend` password
  putStrLn $ "Cleartext:\n" `mappend` clearText
  putStrLn $ "Sum:\t" `mappend` show sum
