module TicTacToe.Game where

import TicTacToe.Types
import TicTacToe.Helpers.Http
import Data.Maybe
import System.Random
import TicTacToe.MExpr.Serializer
import TicTacToe.MExpr.Parser
import Data.List
import Control.Monad

openingMove :: GameArgs -> IO ()
openingMove ga = do
  rm <- randomMove
  postMoves ga [rm]

randomMove :: IO (Move)
randomMove = randomRIO (0, 8) >>= (\i -> return $ [Move x y 'x' | x <- [0..2], y <- [0..2]] !! i)

oneTurn :: GameArgs -> IO ()
oneTurn ga@(_,mode) = do 
  moves <- getMoves ga
  if isOver moves then return ()
  else do
    let best = findBestMove ga moves
    let movesNew = moves ++ [best]
    postMoves ga movesNew
    if isOver movesNew then return () else oneTurn ga

findBestMove :: GameArgs -> Moves -> Move
findBestMove ga moves =
  let player = findPlayer ga moves
      possibleMoves = filter (possibleMove moves) [Move x y player | x <-[0..2], y <-[0..2]]
      movesScores = map (\m -> (m, calculateScore (m:moves) (otherPlayer player) (-1))) possibleMoves
      in fst $ foldl (\acc cur -> if snd cur > snd acc then cur else acc) (Move 0 0 'o', -2) movesScores

findPlayer :: GameArgs -> Moves -> Player
findPlayer (_, Attack) _ = 'x'
findPlayer _ moves
  | length moves < 2 * (length movesX) = 'o'
  | otherwise = 'x'
  where movesX = filter (\m -> player m == 'x') moves

isOver :: Moves -> Bool
isOver moves = (isWinner moves) || (isDraw moves)

isWinner :: Moves -> Bool
isWinner m = fullX m 0 || fullX m 1 || fullX m 2 || fullY m 0 || fullY m 1 || fullY m 2 || fullMainD m || fullMinorD m

fullX :: Moves -> Int -> Bool
fullX moves index = fullLane $ filter (\m -> x m == index) moves

fullY :: Moves -> Int -> Bool
fullY moves index = fullLane $ filter (\m -> y m == index) moves

fullLane :: Moves -> Bool
fullLane moves = length moves == 3 && player (moves !! 0) == player (moves !! 1) && player (moves !! 1) == player (moves !! 2)

fullMainD :: Moves -> Bool
fullMainD moves = fullLane $ filter (\m -> x m == y m) moves

fullMinorD :: Moves -> Bool
fullMinorD moves = fullLane $ filter (\m -> x m + y m == 2) moves


isDraw :: Moves -> Bool
isDraw moves
  | isWinner moves = False
  | otherwise = length moves == 9

possibleMove :: Moves -> Move -> Bool
possibleMove moves (Move xx yy _) = case find (\m -> x m == xx && y m == yy) moves of
  Nothing -> True
  otherwise -> False

calculateScore :: Moves -> Player -> Int -> Int
calculateScore moves _ winScore
  | isWinner moves = -winScore
  | isDraw moves = 0
calculateScore moves player winScore =
  let possibleMoves = filter (possibleMove moves) [Move x y player | x <-[0..2], y <-[0..2]]
      scores = map (\m -> calculateScore (m : moves) (otherPlayer player) (-winScore)) possibleMoves
      resultFunction 1 = maximum
      resultFunction _ = minimum 
  in (resultFunction winScore) scores

otherPlayer :: Player -> Player
otherPlayer 'x' = 'o'
otherPlayer _ = 'x'


postMoves :: GameArgs -> Moves -> IO ()
postMoves ga moves = httpPost (getUrl ga) (serialize moves)

getMoves :: GameArgs -> IO (Moves)
getMoves = fmap parse . httpGet . getUrl