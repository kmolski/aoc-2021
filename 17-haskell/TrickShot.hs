#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import System.Environment (getArgs)

type TargetArea = (Integer, Integer, Integer, Integer)

parseInput :: T.Text -> TargetArea
parseInput line =
  let [x, y] = T.splitOn "," line
      [xLow, xHigh] = T.splitOn ".." x
      [yLow, yHigh] = T.splitOn ".." y
      parse = read . T.unpack . T.dropAround (`notElem` ("0123456789-" :: String))
   in (parse xLow, parse xHigh, parse yLow, parse yHigh)

solvePart1 :: TargetArea -> Integer
solvePart1 (_, _, yLow, _) = (n * (n + 1)) `div` 2 where n = yLow

crossesTargetArea :: TargetArea -> (Integer, Integer) -> (Integer, Integer) -> Bool
crossesTargetArea (_, xh, yl, _) pos@(x, y) _ | x > xh || y < yl = False
crossesTargetArea (xl, xh, yl, yh) (x, y) _ | xl <= x && x <= xh && yl <= y && y <= yh = True
crossesTargetArea target (x, y) (vx, vy) = crossesTargetArea target (x + vx, y + vy) (max 0 $ vx - 1, vy - 1)

solvePart2 :: TargetArea -> Int
solvePart2 coords@(_, xHigh, yLow, _) = length $ filter (crossesTargetArea coords (0, 0)) velocities
  where
    velocities = [(x, y) | x <- [0 .. xHigh], y <- [yLow .. xHigh]]

main :: IO ()
main = do
  (filename : _) <- getArgs
  coords <- parseInput . T.pack <$> readFile filename
  putStrLn $ "Part 1: " ++ show (solvePart1 coords)
  putStrLn $ "Part 2: " ++ show (solvePart2 coords)