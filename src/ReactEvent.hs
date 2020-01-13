module ReactEvent where

import DataStructures
import Graphics.Gloss.Interface.Pure.Game
import Snake (directionSnake)

reactEvent :: Event -> State -> State
reactEvent (EventResize size)                        s = s{winSize = size}
-- Wasd keys
reactEvent (EventKey (Char 'w')            Down _ _) s = changeDirection s North
reactEvent (EventKey (Char 'a')            Down _ _) s = changeDirection s West
reactEvent (EventKey (Char 's')            Down _ _) s = changeDirection s South
reactEvent (EventKey (Char 'd')            Down _ _) s = changeDirection s East
-- Arrow Keys
reactEvent (EventKey (SpecialKey KeyUp)    Down _ _) s = changeDirection s North
reactEvent (EventKey (SpecialKey KeyDown)  Down _ _) s = changeDirection s South
reactEvent (EventKey (SpecialKey KeyLeft)  Down _ _) s = changeDirection s West
reactEvent (EventKey (SpecialKey KeyRight) Down _ _) s = changeDirection s East
-- Vim keys
reactEvent (EventKey (Char 'h')            Down _ _) s = changeDirection s West
reactEvent (EventKey (Char 'j')            Down _ _) s = changeDirection s South
reactEvent (EventKey (Char 'k')            Down _ _) s = changeDirection s North
reactEvent (EventKey (Char 'l')            Down _ _) s = changeDirection s East
reactEvent _ s = s

changeDirection :: State -> Move -> State
changeDirection s m | isOposite (directionSnake(snake s)) m = s
                    | otherwise              = s{action = m}

isOposite:: Move -> Move -> Bool
isOposite North South = True
isOposite South North = True
isOposite East  West  = True
isOposite West  East  = True
isOposite _    _      = False
