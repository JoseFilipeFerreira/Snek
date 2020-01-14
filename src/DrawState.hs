module DrawState where

import DataStructures
import Grid (isWall, isFruit)
import Data.Maybe
import Data.List
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Snake

drawState :: State -> Picture
drawState s | (menu s) == MenuPlay  = drawStatePlay s
            | (menu s) == MenuPause = Pictures[drawStatePlay s, Translate (-200) 0 $ scale 1 1 $ text $ "PAUSE"]
            | otherwise = Translate (-200) 0 $ scale 1 1 $ text $ "Game Over"

-- | Draw the current State
drawStatePlay :: State -> Picture
drawStatePlay s = Scale 0.4 0.4 $ Pictures[ drawMap side (grid s), drawFruit side (grid s), drawSnake side (snake s), debugText]
    where
        debugText = Translate (-200) 0 $ scale 1 1 $ text $ show $ points s
        side = min bv bh
        bv = fromIntegral(fst(winSize s)) / fromIntegral(length(grid s))
        bh = fromIntegral(snd(winSize s)) / fromIntegral(length(head(grid s)))

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
