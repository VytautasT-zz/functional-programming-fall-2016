module TicTacToe.MExpr.Parser where

import Data.Char
import Data.List
import TicTacToe.Types

parse :: String -> Moves 
parse = map (\(a,b,c) -> Move a b c) . parseMoves . map toLower . filter (/=' ')

parseMoves :: String -> [(Int, Int, Char)]
parseMoves ('l':'[':rest) = reverse $ fst $ parseMovesReverse [] rest
parseMoves str = error ("Invalid M-expression " ++ str)

parseMovesReverse :: [(Int, Int, Char)] -> String -> ([(Int, Int, Char)], String)
parseMovesReverse moves "]" = (moves, "")
parseMovesReverse moves str =
  let 
    (move, afterMove) = parseMove str
    afterSeparator = parseSeparator afterMove
  in
    parseMovesReverse (move : moves) afterSeparator 

parseSeparator :: String -> String
parseSeparator "]" = "]"
parseSeparator (';':rest) = rest

parseMove :: String -> ((Int, Int, Char), String)
parseMove ('m':'[':rest) = 
  let (keyValues, afterMove) = parseKeyValues 1 [] rest  
  in (keyValuesToMove keyValues, afterMove)
parseMove _ = error "Move should start with m[" 

keyValuesToMove :: [(Char, Char)] -> (Int, Int, Char)
keyValuesToMove lst = (findPosition 'x' lst, findPosition 'y' lst, findValue 'v' lst)

findPosition :: Char -> [(Char, Char)] -> Int
findPosition key lst = toPosition $ findValue key lst

findValue :: Char -> [(Char, Char)] -> Char
findValue key lst = 
  case find (\p -> fst p == key) lst of
    Nothing -> error ("Could not find key " ++ [key])
    Just pair -> snd pair

toPosition :: Char -> Int
toPosition n
  | n `elem` "012" = digitToInt n
  | otherwise = error ("Ivalid position " ++ [n])


parseKeyValues :: Int -> [(Char, Char)] -> String -> ([(Char, Char)], String)
parseKeyValues 4 keyValues str = (keyValues, str)
parseKeyValues index keyValues str =
    let (nextKeyVal, rest) = parseKeyValue index str
    in  parseKeyValues (index+1) (nextKeyVal:keyValues) rest

parseKeyValue :: Int -> String -> ((Char, Char), String)
parseKeyValue index ('"':key:'"':';':'"':value:'"':delimiter:rest)
  | value /= 'o' && value /= 'x' = error "Only values x or o can be in double quotes"
  | isDelimiterCorrect index delimiter = ((key, value), rest)
parseKeyValue index ('"':key:'"':';':value:delimiter:rest)
  | isDelimiterCorrect index delimiter = ((key, value), rest)
parseKeyValue _ str = error ("Invalid key-value pair: " ++ str)

isDelimiterCorrect :: Int -> Char -> Bool
isDelimiterCorrect index delimiter = (index < 3 && delimiter == ';') || (index == 3 && delimiter == ']')