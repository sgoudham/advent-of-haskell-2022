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

convertToShape :: T.Text -> Shape
convertToShape x
  | x == "A" || x == "X" = Rock
  | x == "B" || x == "Y" = Paper
  | x == "C" || x == "Z" = Scissors
  | otherwise = error $ show x

convertToOutcome :: T.Text -> Outcome
convertToOutcome "X" = Lose
convertToOutcome "Y" = Draw
convertToOutcome "Z" = Win

parseInput :: T.Text -> [(Shape, T.Text)]
parseInput input = map someFunc . T.lines $ input -- ["A Y", "B X"] -> [(Rock, Paper), (Paper, Scissors)]
  where
    someFunc :: T.Text -> (Shape, T.Text)
    someFunc line = (convertToShape first, second)
      where
        [first, second] = T.words $ line

score :: Shape -> Int
score Rock = 1
score Paper = 2
score Scissors = 3

part1 :: [(Shape, T.Text)] -> Int
part1 input = sum $ map (\(opp, player) -> outcomeScore (opp, player) + score player) input'
  where
    input' = map (B.second convertToShape) input

outcomeScore :: (Shape, Shape) -> Int
outcomeScore (opp, player)
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

part2 :: [(Shape, T.Text)] -> Int
part2 input = sum $ map (\(opp, player) -> outcomeScore (opp, player) + score player) shapeList
  where
    outcomeList = map (B.second convertToOutcome) input -- [(Shape, Outcome)]
    shapeList = map outcomeToShape outcomeList
    outcomeToShape (shape, outcome) =
      let outcome' =
            case outcome of
              Win -> beats shape
              Lose -> loses shape
              Draw -> shape
       in (shape, outcome')
