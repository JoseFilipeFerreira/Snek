module ReactTime where

import DataStructures
import Snake (contextualMove)
import Grid  (updateGrid)
import Data.Maybe

reactTime :: Float -> State -> State
reactTime tick s | isJust newSnake = s{ snake = fromJust newSnake
                                      , grid = updateGrid (grid s) (fromJust newSnake)
                                      } 
                | otherwise = s{menu = MenuLost}
    where
        newSnake = contextualMove (grid s) (snake s) (action s)
