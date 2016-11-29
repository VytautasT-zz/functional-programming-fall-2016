module TicTacToe.MExpr.Serializer where

import Data.List
import TicTacToe.Types

serialize :: Moves -> String
serialize lst = "l[" ++ intercalate ";" (map serializeMove lst) ++ "]"

serializeMove :: Move -> String
serializeMove (Move x y player) = "m[\"x\";" ++ (show x) ++ ";\"y\";" ++ (show y) ++ ";\"v\";\"" ++ [player] ++ "\"]"