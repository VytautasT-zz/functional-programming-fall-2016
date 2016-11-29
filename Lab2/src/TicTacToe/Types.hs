module TicTacToe.Types where

type Player = Char

data Move = Move { 
    x :: Int
  , y :: Int
  , player :: Player 
} deriving (Show, Eq)

type Moves = [Move]

data Mode = Attack | Defend
  deriving (Eq)

instance Show Mode where
  show Attack = "1"
  show Defend = "2"

type GameArgs = (String, Mode)