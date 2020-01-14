module ReactTime where

import DataStructures
import Snake (contextualMove)
import Grid  (updateGrid)
import Data.Maybe

reactTime :: Float -> State -> State
reactTime tick s | menu s == MenuPause               = s
                 | isJust newSnake && isJust newGrid = s{ snake = fromJust newSnake
                                                        , grid = fromJust newGrid
                                                        , points = 1 + points s
                                                        }
                 | isJust newSnake                   = s{ snake = fromJust newSnake}
                 | otherwise = s{menu = MenuLost}
    where
        newSnake   = contextualMove (grid s) (snake s) (action s)
        newGrid = updateGrid (grid s) (fromJust newSnake)
