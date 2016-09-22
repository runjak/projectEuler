module Problems where
{-|
  This module is used to generate the Main module.
  This is useful because of the many Problem modules
  that need to be included.
  It's also not possible at the moment[1] to use TemplateHaskell for this.
  [1]: https://ghc.haskell.org/trac/ghc/ticket/1475
|-}
import Data.Function (on)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified System.Directory as Directory
import Text.Regex.Posix as Posix

problems :: IO [String]
problems = do
  dirs <- Directory.listDirectory "src/"
  let hasForm = (=~ "Problem[0-9]+\\.hs")
      cutSuffix = List.takeWhile (/= '.')
      wanted = cutSuffix <$> filter hasForm dirs
      getNumber = read . filter Char.isNumber :: String -> Int
      sorted = fmap snd . List.sortBy (compare `on` fst) $ zip (fmap getNumber wanted) wanted
  return sorted

mainModule :: IO String
mainModule = do
  ps <- problems
  let mkImport = \p -> concat ["import qualified ", p, " as ", p]
      mkPEntry = \p -> concat ["(\"", p, ": \", ", p ,".main)"]
      imports = fmap mkImport ps
      pEntries = List.intercalate ",\n            " $ fmap mkPEntry ps
      pList = ["problems :: [(String, IO ())]",
               "problems = [" `mappend` pEntries `mappend` "]"]
  return . unlines $ concat [
      ["module Main where",
       "",
       "import Control.Monad (forM_)"],
      imports,
      [""],
      pList,
      ["",
       "main = forM_ problems $ \\(desc, p) -> putStr desc >> p"]
    ]

main = writeFile "src/Main.hs" =<< mainModule
