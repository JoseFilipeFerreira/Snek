module Main where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game
import DataStructures
import System.Random
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
        (InWindow "SnEk" (1280, 720) (0, 0))
        (greyN 0.1)
        10
        s
        drawState
        reactEvent
        reactTime

initialState :: Int -> IO State
initialState s = do
                gen <- getStdGen
                newStdGen
                genT <- getStdGen
                return State { grid = fst $ addFruit genT $ generateBasicGrid s
                             , snake = initialSnake s
                             , action = East
                             , points = 0
                             , menu = MenuPlay
                             , rng = gen
                             , winSize = (0, 0)
                             }
