module TicTacToe.Helpers.Args where

import TicTacToe.Types
import System.Environment(getArgs) 
import Data.Char

args :: IO GameArgs
args = toGameArgs <$> getArgs

toGameArgs :: [String] -> GameArgs
toGameArgs [id, t] 
  | noWhitespace == "1" = (id, Attack)
  | noWhitespace == "2" = (id, Defend)
  where noWhitespace = filter (not . isSpace) t
toGameArgs _ = error "Illegal arguments: provide game_id and mode (1 or 2)"