module DrawState where

import DataStructures
import Grid (isWall, isFruit)
import Data.Maybe
import Data.List
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

drawState :: State -> Picture
drawState s | (menu s) == MenuPlay = drawStatePlay s
            | otherwise = Translate (-200) 0 $ scale 1 1 $ text $ "Game Over"

-- | Draw the current State
drawStatePlay :: State -> Picture
drawStatePlay s = Pictures[ drawMap side (grid s), drawFruit side (grid s), drawSnake side (snake s), debugText]
    where
        n = length $ grid s
        debugText = Translate (-200) (-500) $ scale 0.5 0.5 $ text $ show (head (snake s)) ++ show(filter (isFruit (grid s)) $ concat [[(x,y) | x <- [0..n-1] ] | y <- [0..n-1]])

        side = 10

drawSnake:: Float -> Snake -> Picture
drawSnake s m = Pictures $ map (drawBlock red s) m

drawMap:: Float -> Grid -> Picture
drawMap = drawBlocks isWall black

drawFruit:: Float -> Grid -> Picture
drawFruit = drawBlocks isFruit orange

drawBlocks :: (Grid -> Position -> Bool) -> Color -> Float -> Grid -> Picture
drawBlocks f c s m = pictures $ map (drawBlock c s) pos
    where
        n = length m
        pos = filter (f m) $ concat [[(x,y) | x <- [0..n-1] ] | y <- [0..n-1]]

drawBlock :: Color -> Float -> Position -> Picture
drawBlock c s (xi, yi) = Color c $ Polygon [(x, y), (x, y+s), (x+s,y+s), (x+s,y)]
    where
        x = s * fromIntegral xi
        y = s * fromIntegral yi
