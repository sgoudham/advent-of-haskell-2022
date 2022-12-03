{-# LANGUAGE OverloadedStrings #-}

module Days.Day02 (day02) where

import AOC (Solution (..))
import Data.Bifunctor qualified as B
import Data.Text qualified as T

day02 :: Solution
day02 = Solution parseInput part1 part2

data Shape = Rock | Paper | Scissors
  deriving (Eq)

data Outcome = Win | Lose | Draw
  deriving (Eq)

toShape :: T.Text -> Shape
toShape c
  | c == "A" || c == "X" = Rock
  | c == "B" || c == "Y" = Paper
  | c == "C" || c == "Z" = Scissors
  | otherwise = error $ show c

toOutcome :: T.Text -> Outcome
toOutcome "X" = Lose
toOutcome "Y" = Draw
toOutcome "Z" = Win
toOutcome c = error $ show c

pScore :: Shape -> Int
pScore Rock = 1
pScore Paper = 2
pScore Scissors = 3

oScore :: (Shape, Shape) -> Int
oScore (opp, player)
  | player == beats opp = 6
  | player == loses opp = 0
  | otherwise = 3

beats :: Shape -> Shape
beats Rock = Paper
beats Paper = Scissors
beats Scissors = Rock

loses :: Shape -> Shape
loses Rock = Scissors
loses Paper = Rock
loses Scissors = Paper

scores :: [(Shape, Shape)] -> [Int]
scores = map (\(o, p) -> oScore (o, p) + pScore p)

parseInput :: T.Text -> [(Shape, T.Text)]
parseInput = map convert . T.lines
  where
    convert line = let [fst, snd] = T.words line in (toShape fst, snd)

part1 :: [(Shape, T.Text)] -> Int
part1 input = sum $ scores $ map (B.second toShape) input

part2 :: [(Shape, T.Text)] -> Int
part2 input = sum $ scores $ map tupleToShape outcomes
  where
    outcomes = map (B.second toOutcome) input
    tupleToShape (shape, Win) = (shape, beats shape)
    tupleToShape (shape, Lose) = (shape, loses shape)
    tupleToShape (shape, _) = (shape, shape)
