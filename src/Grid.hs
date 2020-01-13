module Grid where

import DataStructures
import System.Random
import System.IO.Unsafe

generateBasicGrid:: Int -> Grid
generateBasicGrid n = [replicate n Wall] ++ replicate (n-2) ([Wall] ++ (replicate (n-2) (Clear)) ++ [Wall]) ++ [replicate n Wall]

addFruit :: Grid -> Grid
addFruit g = updateMatrix (unsafePerformIO(randomRIO(1, (length g) - 1)) ,unsafePerformIO(randomRIO(1, (length g) - 1))) Fruit g

updateGrid :: Grid -> Snake -> Grid
updateGrid g s | isFruit g (head s) = addFruit $ updateMatrix (head s) Clear g
               | otherwise          = g

isFruit::Grid -> Position -> Bool
isFruit g p = Fruit == getPiece g p

isWall::Grid -> Position -> Bool
isWall g p = Wall == getPiece g p

isAlive::Grid -> Position -> Bool
isAlive g p = getPiece g p /= Wall

getPiece::Grid -> Position -> Piece
getPiece g (x, y) = (g !! y) !! x

updateMatrix :: (Int, Int) -> a -> [[a]] -> [[a]]
updateMatrix (c, r) x m =
  take r m ++
  [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++
  drop (r + 1) m
