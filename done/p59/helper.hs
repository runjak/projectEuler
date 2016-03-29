module Helper where

import qualified Data.Map as M
import qualified Data.List as L

type CMap = M.Map Char Int

startMap :: CMap
startMap = M.empty

addChar :: CMap -> Char -> CMap
addChar map c = M.alter inc c map
  where
    inc :: Maybe Int -> Maybe Int
    inc Nothing = Just 1
    inc (Just x) = Just (x + 1)

scanText :: String -> CMap
scanText = foldl addChar startMap

examine :: CMap -> [(Char, Int)]
examine = L.sortBy (\x y -> compare (snd x) (snd y)) . M.assocs

helper :: String -> [(Char, Int)]
helper = examine . scanText
