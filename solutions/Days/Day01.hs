{-# LANGUAGE OverloadedStrings #-}

module Days.Day01 (day01) where

import AOC (Solution (..))
import Data.List (sort)
import Data.Text qualified as T
import Data.Text.Read qualified as T

day01 :: Solution
day01 = Solution parseInput part1 part2

parseInput :: T.Text -> [[Int]]
-- parseInput caloriesString = T.splitOn "\n\n" caloriesString
parseInput caloriesString = intList
  where
    split = T.splitOn "\n\n" caloriesString
    strList = map T.lines split
    intList = map (\x -> map unwrap (map T.decimal x)) strList
    unwrap intList' = case intList' of
      Left a -> error a
      Right (b, _) -> b

part1 :: [[Int]] -> Int
part1 list = maximum $ map (\x -> sum x) list

part2 :: [[Int]] -> Int
part2 list = sum $ take 3 $ reverse $ sort $ map (\x -> sum x) list
