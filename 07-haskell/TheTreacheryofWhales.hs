#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Data.List (minimumBy)
import Data.Ord (comparing)
import qualified Data.Text as T
import System.Environment (getArgs)

parseInput :: [T.Text] -> [Integer]
parseInput = map (read . T.unpack)

type CostFn = Integer -> Integer -> Integer

fuelCostPart1 :: CostFn
fuelCostPart1 pos target = abs (pos - target)

fuelCostPart2 :: CostFn -- sum of 1 + 2 + 3 + 4 + ... series
fuelCostPart2 pos target = (n * (n + 1)) `div` 2 where n = fuelCostPart1 pos target

computeFuelCost :: CostFn -> [Integer] -> Integer -> Integer
computeFuelCost costFn positions target = sum fuelCosts
  where
    fuelCosts = map (costFn target) positions

solve :: CostFn -> [Integer] -> Integer
solve costFn positions = minimum costs
  where
    maxPosition = maximum positions
    costs = map (computeFuelCost costFn positions) [0 .. maxPosition]

main :: IO ()
main = do
  (filename : _) <- getArgs
  fields <- (parseInput <$> T.splitOn ",") . T.pack <$> readFile filename
  putStrLn $ "Part 1: " ++ show (solve fuelCostPart1 fields)
  putStrLn $ "Part 2: " ++ show (solve fuelCostPart2 fields)