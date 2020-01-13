module DataStructures where

data State = State
    { grid   :: Grid
    , snake  :: Snake
    , action :: Move
    , points :: Int
    , menu   :: Menu
    , winSize :: (Int, Int)
    }
  deriving (Eq, Show)

data Menu = MenuPlay | MenuLost
    deriving (Eq, Show)

data Piece = Clear | Wall | Fruit
    deriving (Eq, Show)

type Grid = [[Piece]]

type Position = (Int, Int)

type Snake = [Position]

data Move = North | South | East | West
  deriving (Eq, Show)
