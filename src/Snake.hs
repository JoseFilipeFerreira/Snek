module Snake where

import Data.List
import Data.Maybe
import DataStructures
import Grid (isWall, isFruit)

initialSnake::Int -> Snake
initialSnake n = [(c+2,c), (c+1,c),(c,c)]
    where
        c = div n 2

contextualMove :: Grid -> Snake -> Move -> Maybe Snake
contextualMove g s m | isWall  g (head newSnake) || not(validSnake s) = Nothing
                     | isFruit g (head newSnake) = Just newSnake
                     | otherwise = Just $ init newSnake
    where
        newSnake = moveSnake s m 

moveSnake:: Snake -> Move -> Snake
moveSnake s@((x,y):_) m | m == North = (x, y+1) : s
                        | m == South = (x, y-1) : s
                        | m == East  = (x+1, y) : s
                        | m == West  = (x-1, y) : s

directionSnake :: Snake -> Move
directionSnake s | x == 0 && y <  0 = North 
                 | x == 0 && y >  0 = South
                 | x >  0 && y == 0 = West
                 | x <  0 && y == 0 = East
    where
        x = fst(s !! 1) - fst(s !! 0)
        y = snd(s !! 1) - snd(s !! 0)

validSnake:: Snake -> Bool
validSnake = allUnique

allUnique :: Ord a => [a] -> Bool
allUnique = all ( (==) 1 . length) . group . sort
