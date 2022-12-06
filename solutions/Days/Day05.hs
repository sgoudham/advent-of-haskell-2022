{-# LANGUAGE OverloadedStrings #-}

module Days.Day05 (day05) where

import AOC (Solution (..))
import Data.Char
import Data.List qualified as L
import Data.List.Split qualified as S
import Data.Text qualified as T

-- 1. How many crates to move
-- 2. The stack to move crates from
-- 3. The stack to move crates to
type Instruction = (Int, Int, Int)

mapTuple :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTuple f (a1, a2, a3) = (f a1, f a2, f a3)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x : xs)
  | n == 0 = newVal : xs
  | otherwise = x : replaceNth (n - 1) newVal xs

day05 :: Solution
day05 = Solution parseInput part1 part2

parseInput :: T.Text -> ([String], [Instruction])
parseInput input = (cleanStacks, intInsts)
  where
    [crates, insts] = S.splitOn "\n\n" (T.unpack input)
    inst = filter (not . null) $ map (filter isDigit) (words insts)
    intInsts = map (\[one, two, three] -> mapTuple read (one, two, three)) (S.chunksOf 3 inst)
    stacks = init $ lines crates
    cleanStacks = map concat $ L.transpose $ map (map filtered . S.chunksOf 4) stacks
    filtered xs = [xs !! 1 | xs !! 1 /= ' ']

part1 :: ([String], [Instruction]) -> String
part1 (stacks, instructions) = map head (foldl logic stacks instructions)
  where
    logic :: [String] -> Instruction -> [String]
    logic stacks' (num, from, to) = replacedStacks
      where
        to' = to - 1
        from' = from - 1
        sFrom = stacks' !! from'
        sTo = stacks' !! to'
        taken = replaceNth from' (drop num sFrom) stacks'
        replacedStacks = replaceNth to' (reverse (take num sFrom) ++ sTo) taken

part2 :: ([String], [Instruction]) -> String
part2 (stacks, instructions) = map head (foldl logic stacks instructions)
  where
    logic :: [String] -> Instruction -> [String]
    logic stacks' (num, from, to) = replacedStacks
      where
        to' = to - 1
        from' = from - 1
        sFrom = stacks' !! from'
        sTo = stacks' !! to'
        taken = replaceNth from' (drop num sFrom) stacks'
        replacedStacks = replaceNth to' (take num sFrom ++ sTo) taken
