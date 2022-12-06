module Days.Day06 (day06) where

import AOC (Solution (..))
import Data.List
import Data.Maybe
import Data.Text qualified as T

day06 :: Solution
day06 = Solution parseInput part1 part2

isUnique :: String -> Bool
isUnique str = length (nub str) == length str

detectUnique :: Int -> String -> Char -> String
detectUnique n acc x =
  if isUnique (take n $ reverse acc)
    then acc ++ "0"
    else acc ++ [x]

parseInput :: T.Text -> String
parseInput = T.unpack

part1 :: String -> Int
part1 input = fromJust $ elemIndex '0' (foldl (detectUnique chars) accumulator iterable)
  where
    chars = 4
    accumulator = take chars input
    iterable = drop chars input

part2 :: String -> Int
part2 input = fromJust $ elemIndex '0' (foldl (detectUnique chars) accumulator iterable)
  where
    chars = 14
    accumulator = take chars input
    iterable = drop chars input
