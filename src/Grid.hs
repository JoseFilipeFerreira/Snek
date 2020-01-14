module Grid where

import DataStructures
import Data.Maybe
import System.Random
import System.IO.Unsafe

generateBasicGrid:: Int -> Grid
generateBasicGrid n = [replicate n Wall] ++ replicate (n-2) ([Wall] ++ (replicate (n-2) (Clear)) ++ [Wall]) ++ [replicate n Wall]

addFruit :: StdGen -> Grid -> (Grid, StdGen)
addFruit gen g = (updateMatrix pos Fruit g, ngen)
    where
        l = length g
        (pos, ngen) = randomPosition gen ((1, l -2), (1, l -2))

updateGrid :: StdGen -> Grid -> Snake -> Maybe (Grid, StdGen)
updateGrid gen g s | isFruit g (head s) = Just $ addFruit gen $ updateMatrix (head s) Clear g
                   | otherwise          = Nothing

isFruit::Grid -> Position -> Bool
isFruit g p = Fruit == getPiece g p

isWall::Grid -> Position -> Bool
isWall g p = Wall == getPiece g p

isAlive::Grid -> Position -> Bool
isAlive g p = getPiece g p /= Wall

getPiece::Grid -> Position -> Piece
getPiece g (x, y) = (g !! y) !! x

randomPosition :: StdGen -> (Position,Position) -> (Position, StdGen)
randomPosition rng (x, y) = ((rx, ry), fgen)
    where
        (rx, ngen) = randomR x rng
        (ry, fgen) = randomR y ngen

updateMatrix :: (Int, Int) -> a -> [[a]] -> [[a]]
updateMatrix (c, r) x m =
  take r m ++
  [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++
  drop (r + 1) m
