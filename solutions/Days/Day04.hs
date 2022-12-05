{-# LANGUAGE OverloadedStrings #-}

module Days.Day04 (day04) where

import AOC (Solution (..))
import Data.Text qualified as T

day04 :: Solution
day04 = Solution parseInput part1 part2

parseInput :: T.Text -> [[[Int]]]
parseInput = map (map (map (read . T.unpack) . T.splitOn "-") . T.splitOn ",") . T.lines

part1 :: [[[Int]]] -> Int
part1 input = sum $ map logic input
  where
    logic [[u1, u2], [d1, d2]]
      | u1 >= d1 && u2 <= d2 = 1
      | d1 >= u1 && d2 <= u2 = 1
      | otherwise = 0

part2 :: [[[Int]]] -> Int
part2 input = sum $ map logic input
  where
    logic [[u1, u2], [d1, d2]]
      | u1 >= d1 && u1 <= d2 = 1
      | u2 >= d1 && u2 <= d2 = 1
      | d1 >= u1 && d1 <= u2 = 1
      | d2 >= u1 && d2 <= u2 = 1
      | otherwise = 0
