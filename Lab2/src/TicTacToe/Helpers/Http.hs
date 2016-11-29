{-# LANGUAGE OverloadedStrings #-}
module TicTacToe.Helpers.Http
where

import Network.Wreq as Wreq
import Control.Lens
import Data.ByteString.Char8 as Char8
import Data.ByteString.Lazy as LBStr
import TicTacToe.Types

getUrl :: GameArgs -> String
getUrl (id, mode) =
  "http://tictactoe.homedir.eu/game/" ++
  id ++ 
  "/player/" ++ 
  show mode

httpGet :: String -> IO String
httpGet url = do
  let opts = defaults & header "Accept" .~ ["application/m-expr"]
  r <- getWith opts url
  return $ Char8.unpack $ LBStr.toStrict $ r ^. Wreq.responseBody

httpPost :: String -> String -> IO ()
httpPost url message = do
  let opts = defaults & header "Content-Type" .~ ["application/m-expr"]
  r <- postWith opts url $ Char8.pack message
  return ()
  