module Main where

import Data.List (nub,delete,(\\),minimumBy)
import Data.Char (isDigit)
import qualified Data.Map as M
import Control.Monad (guard)
import System.IO (readFile)

{- ----- ----- ----- ----- ----- types and data ----- ----- ----- ----- ----- -}

type Point = (Int,Int)
type Element = [Point]

data SudokuTile = SudokuTile {
   possibleNumbers :: [Int]
  ,point           :: Point
  ,concluded       :: Bool -- | helps remembering which fields are already visited, default is False
} deriving (Show, Eq)

type Field = M.Map Point SudokuTile
type STF = (SudokuTile -> SudokuTile)

{- ----- ----- ----- ----- ----- usefull functions for data ----- ----- ----- ----- ----- -}

setField :: SudokuTile -> Int -> SudokuTile
setField tile x = tile{possibleNumbers=[x]}

setConclusion :: Bool -> SudokuTile -> SudokuTile
setConclusion b t = t{concluded=b}

disableField :: SudokuTile -> Int -> SudokuTile
disableField tile x = tile{possibleNumbers=(delete x $ possibleNumbers tile)}

maybefy :: (a -> a) -> Maybe a -> Maybe a
maybefy f Nothing = Nothing
maybefy f (Just a) = Just $ f a

alter :: STF -> Field -> Element -> Field
alter f = foldl $ alter1 f

alter1 :: STF -> Field -> Point -> Field
alter1 f field = flip (M.alter (maybefy f)) field

points :: Element
points = [(x,y)|x<-[0..8],y<-[0..8]]

elements :: [Element]
elements = blocks ++ cols ++ rows
  where
    blocks = map (\x->x : getBlock x) [(x,y)|x<-[0,3,6],y<-[0,3,6]]
    cols = map (\x->x : getCol x) [(x,0)|x<-[0..8]]
    rows = map (\x->x : getRow x) [(0,y)|y<-[0..8]]

-- | excludes self
getRow :: Point -> Element
getRow (x,y) = [(x',y)|x'<-[0..8],x'/=x]

-- | excludes self
getCol :: Point -> Element
getCol (x,y) = [(x,y')|y'<-[0..8],y'/=y]

-- | excludes self
getBlock :: Point -> Element
getBlock (x,y) =
  let (a,b) = (x `div` 3 * 3, y `div` 3 * 3)
  in [(c,d)|c<-[a..(a+2)],d<-[b..(b+2)],(c,d)/=(x,y)]

-- | excludes self
getAffected :: Point -> Element
getAffected point = nub $ getBlock point ++ (getRow point ++ getCol point)

{- ----- ----- ----- ----- ----- solving algorithm ----- ----- ----- ----- ----- -}
-- | This is not the most elegant, but works :P
exhausive :: (Field -> Field) -> Field -> Field
exhausive f field =
  let next = f field
  in if field /= next
    then exhausive f next
    else field

{- --- --- --- Conclusion --- --- --- -}
conclusion :: Field -> Field
conclusion field = foldl conclude field $ M.elems field

conclude :: Field -> SudokuTile -> Field
conclude field tile -- | Improve: work on affected Fields
  | concludeAble tile =
    let newField = alter1 (setConclusion True) field $ point tile
    in alter (flip disableField $ head $ possibleNumbers tile) newField (getAffected $ point tile)
  | otherwise = field

conclude' :: Field -> Point -> Field
conclude' field = maybe field (conclude field) . flip M.lookup field

concludeAble :: SudokuTile -> Bool
concludeAble x = (not $ concluded x) && ((==1) . length $ possibleNumbers x)

{- --- --- --- Exclusion --- --- --- -}
exclusion :: Field -> Field
exclusion = flip (foldl exclude) elements

-- | takes advantage of conclusion :P
exclude :: Field -> Element -> Field
exclude field e =
  let
    tiles = concatMap (maybe [] return . flip M.lookup field) e :: [SudokuTile]
    set = [(x,delete x tiles)|x<-tiles] :: [(SudokuTile,[SudokuTile])]
  in
    foldl exclude' field set
  where
    exclude' :: Field -> (SudokuTile,[SudokuTile]) -> Field
    exclude' field (t,ts)
      | (length $ possibleNumbers t) <= 1 = field
      | otherwise = check field (point t) . foldl1 (\\) $ possibleNumbers t : map possibleNumbers ts

    check :: Field -> Point -> [Int] -> Field
    check field p [n] = flip conclude' p $ alter1 (flip setField n) field p
    check field _ _ = field

{- --- --- --- Backtracking --- --- --- -}
-- | following multiple paths
backtracking :: Field -> Field
backtracking field = may . filter solved . map solve $ backtrack field
  where
    may [] = field -- | may may fail
    may (f:fs) = f

-- | single backtracking step
backtrack :: Field -> [Field]
backtrack field = do
  let filterList = filter ((>= 2) . length . possibleNumbers) $ M.elems field
  guard (filterList /= [])
  let subject = minimumBy compareTile filterList
  f<-[(flip setField x)|x<-possibleNumbers subject]
  return $ alter1 f field (point subject)
  where
    compareTile :: SudokuTile -> SudokuTile -> Ordering -- | wtf ^^
    compareTile = (. (length . possibleNumbers)) . compare . length . possibleNumbers

{- --- --- --- Solving --- --- --- -}
solve :: Field -> Field
solve = solveList [exhausive conclusion, exhausive exclusion, backtracking]
  where
    solveList :: [(Field->Field)] -> Field -> Field
    solveList [] field = field
    solveList (f:fs) field
      | solved field = field
      | otherwise = solveList fs $ f field

solved :: Field -> Bool
solved field = all (==[]) $ map ((\\)[1..9] . gatherNumbers field) elements
  where
    gatherNumbers :: Field -> Element -> [Int]
    gatherNumbers field = concatMap (helper . maybe [] possibleNumbers . flip M.lookup field)

    helper :: [Int] -> [Int]
    helper [] = [0]
    helper [x] = [x]
    helper x = [0]

impossible :: Field -> Bool
impossible = any ((==0) . length . possibleNumbers) . M.elems

{- ----- ----- ----- ----- ----- Using what we've got ----- ----- ----- ----- ----- -}

file :: IO String
file = readFile "sudoku.txt"

mkChunks :: Int -> [a] -> [[a]]
mkChunks n list
  | length list `mod` n == 0 = mkChunks' list
  | otherwise = []
  where
    mkChunks' [] = []
    mkChunks' x =
      let parts = splitAt n x
      in fst parts : mkChunks' (snd parts)

loadFields :: String -> [Field]
loadFields = map (mkField . mkTiles) . mkChunks 81 . mkIntList
  where
    mkIntList :: String -> [Int]
    mkIntList = concatMap (map (\x->(read :: String -> Int) [x])) . map (filter isDigit) . filter ((/='G') . head) . lines

    mkTiles :: [Int] -> [SudokuTile]
    mkTiles = zipWith (\p n -> SudokuTile (nums n) p False) points
      where
        nums :: Int -> [Int]
        nums x
          | x == 0 = [1..9]
          | otherwise = [x]

    mkField :: [SudokuTile] -> Field
    mkField = M.fromAscList . map ((,) =<< point)

showField :: Field -> String
showField = unlines . mkChunks 9 . concatMap (show . nums . possibleNumbers) . M.elems
  where
    nums :: [Int] -> Int
    nums [x] = x
    nums x = 0

fieldKey :: Field -> Integer
fieldKey = maybe 0 id . helper
  where
    helper :: Field -> Maybe Integer
    helper field = do
      f1 <- M.lookup (0,0) field
      f2 <- M.lookup (0,1) field
      f3 <- M.lookup (0,2) field
      let d1 = show . head $ possibleNumbers f1
      let d2 = show . head $ possibleNumbers f2
      let d3 = show . head $ possibleNumbers f3
      return $ read (d1++d2++d3)

solution :: IO ()
solution = do
  input <- file
  let fields = loadFields input
  let keys = map (fieldKey . solve) fields
  let number = sum keys
  print number
