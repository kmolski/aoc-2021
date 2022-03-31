#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import System.Environment (getArgs)

-- (x, y, z) x (low, high)
type Cuboid = (Integer, Integer, Integer, Integer, Integer, Integer)

-- ("on" | "off", cuboid)
type CubeOp = (T.Text, Cuboid)

parseInput :: T.Text -> CubeOp
parseInput line =
  let [op, cuboid] = T.splitOn " " line
      [[xl, xh], [yl, yh], [zl, zh]] = map (T.splitOn "..") $ T.splitOn "," cuboid
      parse = read . T.unpack . T.dropAround (`notElem` ("0123456789-" :: String))
   in (op, (parse xl, parse xh, parse yl, parse yh, parse zl, parse zh))

(<?) :: Ord a => (a, a) -> (a, a) -> Bool
(<?) (xl, xh) (min, max) = xl >= min && xh <= max

isInside :: Cuboid -> Cuboid -> Bool
isInside (xl1, xh1, yl1, yh1, zl1, zh1) (xl2, xh2, yl2, yh2, zl2, zh2) =
  (xl2, xh2) <? (xl1, xh1) && (yl2, yh2) <? (yl1, yh1) && (zl2, zh2) <? (zl1, zh1)

isInsideInitArea :: CubeOp -> Bool
isInsideInitArea (_, cuboid) = isInside (-50, 50, -50, 50, -50, 50) cuboid

doesCollide :: Cuboid -> Cuboid -> Bool -- AABB intersection detection
doesCollide (xl1, xh1, yl1, yh1, zl1, zh1) (xl2, xh2, yl2, yh2, zl2, zh2) =
  (xh1, xl1) <? (xl2, xh2) && (yh1, yl1) <? (yl2, yh2) && (zh1, zl1) <? (zl2, zh2)

cuboidVolume :: Cuboid -> Integer
cuboidVolume (xl, xh, yl, yh, zl, zh) = (xh - xl + 1) * (yh - yl + 1) * (zh - zl + 1)

getNonOverlappingCuboids :: [Cuboid] -> [Cuboid] -> [Cuboid]
getNonOverlappingCuboids newCuboids (otherCuboid : rest) = getNonOverlappingCuboids unique rest
  where
    unique = concatMap (`subtractCuboids` otherCuboid) newCuboids
getNonOverlappingCuboids newCuboids [] = newCuboids

splitAlongPlaneX :: Cuboid -> Cuboid -> [Cuboid]
splitAlongPlaneX (xl1, xh1, yl1, yh1, zl1, zh1) (xl2, xh2, _, _, _, _) =
  filter
    (\(xl, xh, _, _, _, _) -> xl <= xh && (xl, xh) <? (xl1, xh1))
    [ (xl1, intersectionLower - 1, yl1, yh1, zl1, zh1),
      (intersectionLower, intersectionUpper, yl1, yh1, zl1, zh1),
      (intersectionUpper + 1, xh1, yl1, yh1, zl1, zh1)
    ]
  where
    intersectionLower = max xl1 xl2
    intersectionUpper = min xh1 xh2

splitAlongPlaneY :: Cuboid -> Cuboid -> [Cuboid]
splitAlongPlaneY (xl1, xh1, yl1, yh1, zl1, zh1) (_, _, yl2, yh2, _, _) =
  filter
    (\(_, _, yl, yh, _, _) -> yl <= yh && (yl, yh) <? (yl1, yh1))
    [ (xl1, xh1, yl1, intersectionLower - 1, zl1, zh1),
      (xl1, xh1, intersectionLower, intersectionUpper, zl1, zh1),
      (xl1, xh1, intersectionUpper + 1, yh1, zl1, zh1)
    ]
  where
    intersectionLower = max yl1 yl2
    intersectionUpper = min yh1 yh2

splitAlongPlaneZ :: Cuboid -> Cuboid -> [Cuboid]
splitAlongPlaneZ (xl1, xh1, yl1, yh1, zl1, zh1) (_, _, _, _, zl2, zh2) =
  filter
    (\(_, _, _, _, zl, zh) -> zl <= zh && (zl, zh) <? (zl1, zh1))
    [ (xl1, xh1, yl1, yh1, zl1, intersectionLower - 1),
      (xl1, xh1, yl1, yh1, intersectionLower, intersectionUpper),
      (xl1, xh1, yl1, yh1, intersectionUpper + 1, zh1)
    ]
  where
    intersectionLower = max zl1 zl2
    intersectionUpper = min zh1 zh2

subtractCuboids :: Cuboid -> Cuboid -> [Cuboid]
subtractCuboids a b
  | doesCollide a b =
    let splitX = splitAlongPlaneX a b
        splitY = concatMap (`splitAlongPlaneY` b) splitX
        splitZ = concatMap (`splitAlongPlaneZ` b) splitY
     in filter (not . isInside b) splitZ
  | otherwise = [a]

solve :: [Cuboid] -> [CubeOp] -> Integer
solve existingCuboids (("on", cuboid) : rest) = solve newCuboids rest
  where
    newCuboids = existingCuboids ++ getNonOverlappingCuboids [cuboid] existingCuboids
solve existingCuboids (("off", cuboid) : rest) = solve newCuboids rest
  where
    newCuboids = concatMap (`subtractCuboids` cuboid) existingCuboids
solve cuboids [] = sum $ map cuboidVolume cuboids
solve _ _ = undefined

main :: IO ()
main = do
  (filename : _) <- getArgs
  cubeOps <- map parseInput . T.lines . T.pack <$> readFile filename
  putStrLn $ "Part 1: " ++ show (solve [] $ filter isInsideInitArea cubeOps)
  putStrLn $ "Part 2: " ++ show (solve [] cubeOps)