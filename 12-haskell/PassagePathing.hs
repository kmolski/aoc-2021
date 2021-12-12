#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isLower)
import Data.List (groupBy, sortOn, (\\))
import qualified Data.Map as M
import qualified Data.Text as T
import System.Environment (getArgs)

type Cave = T.Text

type CaveGraph = M.Map Cave [Cave]

groupEdges :: [(Cave, Cave)] -> [(Cave, [Cave])] -- group edges into [(from, [to, ...]), ...]
groupEdges = map (\x -> (fst . head $ x, map snd x)) . groupBy (\a b -> fst a == fst b) . sortOn fst

parseLine :: T.Text -> [(Cave, Cave)] -- parse line to edge in both directions
parseLine line = [(from, to), (to, from)] where (from : to : _) = T.splitOn "-" line

createGraph :: [T.Text] -> CaveGraph
createGraph = M.fromList . groupEdges . concatMap parseLine

isSmallCave :: Cave -> Bool -- true if the cave is small. intentionally includes "start" as small cave
isSmallCave = T.all isLower

isSecondVisit :: [Cave] -> Bool -- true if the last step was a second visit to a small cave
isSecondVisit path@(lastStep : rest) = isSmallCave lastStep && elem lastStep rest

getPathsToEnd :: CaveGraph -> Bool -> [Cave] -> [[Cave]]
getPathsToEnd _ _ path@("end" : _) = [path] -- stop condition
getPathsToEnd graph visitedSecond path@(current : _) =
  let blockedSmallCaves = if not visitedSecond then ["start"] else filter isSmallCave path
      nextPaths = map (: path) $ (graph M.! current) \\ blockedSmallCaves
   in concatMap (\x -> getPathsToEnd graph (visitedSecond || isSecondVisit x) x) nextPaths

main :: IO ()
main = do
  (filename : _) <- getArgs
  graph <- createGraph . T.lines . T.pack <$> readFile filename
  putStrLn $ "Part 1: " ++ show (length (getPathsToEnd graph True ["start"]))
  putStrLn $ "Part 2: " ++ show (length (getPathsToEnd graph False ["start"]))