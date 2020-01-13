module Snake where

import Data.List
import Data.Maybe
import DataStructures
import Grid (isWall, isFruit)

initialSnake::Int -> Snake
initialSnake n = [(1,1), (1,2),(1,3)]

contextualMove :: Grid -> Snake -> Move -> Maybe Snake
contextualMove g s m | isWall  g (head newSnake) || not(validSnake s) = Nothing
                     | isFruit g (head newSnake) = Just newSnake
                     | otherwise = Just $ init newSnake
    where
        newSnake = moveSnake s m 

moveSnake:: Snake -> Move -> Snake
moveSnake s m | m == North = (x, y+1) : s
              | m == South = (x, y-1) : s
              | m == East  = (x+1, y) : s
              | m == West  = (x-1, y) : s
    where
        (x, y) = head s

directionSnake :: Snake -> Move
directionSnake s | x == 0 && y <  0 = North 
                 | x == 0 && y >  0 = South
                 | x >  0 && y == 0 = West
                 | x <  0 && y == 0 = East
    where
        x = fst(s !! 1) - fst(s !! 0)
        y = snd(s !! 1) - snd(s !! 0)

validSnake:: Snake -> Bool
validSnake s = allUnique s

allUnique :: Ord a => [a] -> Bool
allUnique = all ( (==) 1 . length) . group . sort
