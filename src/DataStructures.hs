module DataStructures where

import System.Random

data State = State
    { grid   :: Grid
    , snake  :: Snake
    , action :: Move
    , points :: Int
    , menu   :: Menu
    , rng    :: StdGen
    , winSize :: (Int, Int)
    }
  deriving (Show)

data Menu = MenuPlay | MenuPause | MenuLost
    deriving (Eq, Show)

data Piece = Clear | Wall | Fruit
    deriving (Eq, Show)

type Grid = [[Piece]]

type Position = (Int, Int)

type Snake = [Position]

data Move = North | South | East | West
  deriving (Eq, Show)
