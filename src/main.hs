module Main where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game
import DataStructures
import Grid       (addFruit, generateBasicGrid)
import Snake      (initialSnake)
import ReactEvent (reactEvent)
import ReactTime  (reactTime)
import DrawState  (drawState)

main :: IO ()
main = do initial <- initialState 50
          game initial

game:: State -> IO()
game s = play
        FullScreen
        (greyN 0.8)
        10
        s
        drawState
        reactEvent
        reactTime

initialState :: Int -> IO State
initialState s = do  
                return State { grid = addFruit $ generateBasicGrid s
                             , snake = initialSnake s
                             , action = East
                             , points = 0
                             , menu = MenuPlay
                             , winSize = (0, 0)
                             }
