module Days.Day03 (day03, splitInto, commonItem) where

import AOC (Solution (..))
import qualified Data.Text as T
import Data.List
import Data.Maybe
import Debug.Trace

day03 :: Solution
day03 = Solution parseInput part1 part2

parseInput :: T.Text -> [[String]]
parseInput input = go [] allLines
  where
    go acc [] = reverse acc
    go acc xs = go (take 3 xs : acc) (drop 3 xs)
    allLines = map T.unpack $ T.lines input

priority :: Char -> Int
priority = (+1) . fromJust . flip elemIndex (['a'..'z'] ++ ['A'..'Z'])

splitInto :: Int -> [a] -> [[a]]
splitInto s xs = go [] xs
  where 
    go acc [] = reverse acc
    go acc xs' = go (take takeInt xs' : acc) (drop takeInt xs')
    takeInt = round $ (fromIntegral $ length xs) / (fromIntegral s)

commonItem :: Eq a => [[a]] -> [a]
commonItem [] = []
commonItem xs = foldl1' intersect xs

part1 :: [[String]] -> Int
part1 input = sum $ map (priority . head . commonItem . splitInto 2) (concat input)

part2 :: [[String]] -> Int
part2 input = sum $ map (priority . head . commonItem) input
-- part2 input = sum $ map (sum . map (priority . head . commonItem . splitInto 2)) (traceShowId input)
-- part2 input = traceShowId input 
