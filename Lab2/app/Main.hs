module Main where

import TicTacToe.Game
import TicTacToe.Types
import TicTacToe.Helpers.Args as A
import Control.Monad

main :: IO ()
main = do
    gameArgs@(gameId,mode) <- A.args
    when (mode == Attack) (openingMove gameArgs)  
    oneTurn gameArgs