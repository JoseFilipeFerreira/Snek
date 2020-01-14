module ReactTime where

import DataStructures
import Snake (contextualMove)
import Grid  (updateGrid)
import Data.Maybe

reactTime :: Float -> State -> State
reactTime tick s | menu s == MenuPause                  = s
                 | isJust newSnake && isJust newGridGen = s{ snake = fromJust newSnake
                                                           , grid = fst $ fromJust newGridGen
                                                           , rng = snd $ fromJust newGridGen
                                                           , points = 1 + points s
                                                           }
                 | isJust newSnake                      = s{ snake = fromJust newSnake}
                 | otherwise = s{menu = MenuLost}
    where
        newSnake = contextualMove (grid s) (snake s) (action s)
        newGridGen  = updateGrid (rng s) (grid s) (fromJust newSnake)
