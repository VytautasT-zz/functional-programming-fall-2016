module Main where
 
import Test.Hspec
import TicTacToe.Types
import TicTacToe.MExpr.Parser
import TicTacToe.MExpr.Serializer
import TicTacToe.Game
 
main :: IO ()
main = hspec $ do
  describe "findBestMove" $ do
    it "return a blocking move" $ do
      let gameArgs = ("id", Defend)
      let moves = [ Move {x = 1, y = 1, player = 'x'}
                  , Move {x = 0, y = 0, player = 'o'}
                  , Move {x = 0, y = 2, player = 'x'} ]
      findBestMove gameArgs moves `shouldBe` Move {x = 2, y = 0, player = 'o'}

    it "return a winning move" $ do
      let gameArgs = ("id", Attack)
      let moves = [ Move {x = 1, y = 1, player = 'x'}
                  , Move {x = 0, y = 1, player = 'o'}
                  , Move {x = 1, y = 0, player = 'x'} 
                  , Move {x = 1, y = 2, player = 'o'}
                  , Move {x = 0, y = 0, player = 'x'}
                  , Move {x = 2, y = 0, player = 'o'} ]
      findBestMove gameArgs moves `shouldBe` Move {x = 2, y = 2, player = 'x'}

  describe "isOver" $ do
    it "returns True if game is over" $ do
      let moves = [ Move {x = 0, y = 0, player = 'x'}
                  , Move {x = 1, y = 0, player = 'x'}
                  , Move {x = 2, y = 0, player = 'x'}
                  , Move {x = 1, y = 1, player = 'x'} 
                  , Move {x = 0, y = 1, player = 'o'}
                  , Move {x = 0, y = 2, player = 'o'}
                  , Move {x = 2, y = 2, player = 'o'} ]
      isWinner moves `shouldBe` True

    it "returns False if game is not over yet" $ do
      let moves = [ Move {x = 2, y = 0, player = 'x'}
                  , Move {x = 0, y = 2, player = 'o'} 
                  , Move {x = 1, y = 2, player = 'x'}
                  , Move {x = 2, y = 2, player = 'o'}
                  , Move {x = 0, y = 1, player = 'x'} ]
      isWinner moves `shouldBe` False  


  describe "isWinner" $ do
    it "returns True if there is a winner" $ do
      let moves = [ Move {x = 0, y = 0, player = 'x'}
                  , Move {x = 1, y = 0, player = 'x'}
                  , Move {x = 2, y = 0, player = 'x'}
                  , Move {x = 1, y = 1, player = 'x'} 
                  , Move {x = 0, y = 1, player = 'o'}
                  , Move {x = 0, y = 2, player = 'o'}
                  , Move {x = 2, y = 2, player = 'o'} ]
      isWinner moves `shouldBe` True

    it "returns False if there is no winner" $ do
      let moves = [ Move {x = 2, y = 0, player = 'x'}
                  , Move {x = 0, y = 2, player = 'o'} 
                  , Move {x = 1, y = 2, player = 'x'}
                  , Move {x = 2, y = 2, player = 'o'}
                  , Move {x = 0, y = 1, player = 'x'} ]
      isWinner moves `shouldBe` False

  describe "isDraw" $ do
    it "returns True if board is full and there is no winner" $ do
      let moves = [ Move {x = 0, y = 0, player = 'x'}
                  , Move {x = 1, y = 0, player = 'o'}
                  , Move {x = 2, y = 0, player = 'x'}
                  , Move {x = 0, y = 2, player = 'o'} 
                  , Move {x = 1, y = 2, player = 'x'}
                  , Move {x = 2, y = 2, player = 'o'}
                  , Move {x = 0, y = 1, player = 'x'} 
                  , Move {x = 1, y = 1, player = 'o'}
                  , Move {x = 2, y = 1, player = 'x'} ]
      isDraw moves `shouldBe` True

    it "returns False if board is full but there is a winner" $ do
      let moves = [ Move {x = 0, y = 0, player = 'x'}
                  , Move {x = 1, y = 0, player = 'o'}
                  , Move {x = 2, y = 0, player = 'o'}
                  , Move {x = 0, y = 2, player = 'o'} 
                  , Move {x = 1, y = 2, player = 'x'}
                  , Move {x = 2, y = 2, player = 'x'}
                  , Move {x = 0, y = 1, player = 'x'} 
                  , Move {x = 1, y = 1, player = 'o'}
                  , Move {x = 2, y = 1, player = 'x'} ]
      isDraw moves `shouldBe` False

    it "returns False if board is not full" $ do
      let moves = [ Move {x = 2, y = 0, player = 'x'}
                  , Move {x = 0, y = 2, player = 'o'} 
                  , Move {x = 1, y = 2, player = 'x'}
                  , Move {x = 2, y = 2, player = 'o'}
                  , Move {x = 0, y = 1, player = 'x'} ]
      isDraw moves `shouldBe` False


    it "parses m-expr and returns an array of Move" $ do
      let str = "l[m[\"x\";  2; \"y\"; 1; \"v\";   \"x\"]; m[\"x\"; 0;  \"y\";   0; \"v\"; \"o\"]; m[\"x\";   1;  \"y\";   0; \"v\";   \"x\"]; m[\"x\";  0;  \"y\";  1; \"v\";   \"o\"]]"
      parse str `shouldBe` [ Move {x = 2, y = 1, player = 'x'}
                           , Move {x = 0, y = 0, player = 'o'}
                           , Move {x = 1, y = 0, player = 'x'}
                           , Move {x = 0, y = 1, player = 'o'} ]

    it "parses m-expr and returns an array of without fixed order of keys" $ do
      let str = "l[m[\"y\"; 1; \"v\";  \"x\";  \"x\"; 2  ]; m[\"x\"; 0 ; \"v\"; \"o\" ;   \"y\";   0]]"
      parse str `shouldBe` [Move {x = 2, y = 1, player = 'x'}, Move {x = 0, y = 0, player = 'o'}]


  describe "Serialize m-expression" $ do
    it "serialization is started with l[ ... ]" $ do
      serialize [] `shouldBe` "l[]"

    it "serializes Move array to m-expr string" $ do
      let moves = [ Move {x = 2, y = 1, player = 'x'}
                  , Move {x = 0, y = 0, player = 'o'}
                  , Move {x = 1, y = 0, player = 'x'}
                  , Move {x = 0, y = 1, player = 'o'} ]
      let str = "l[m[\"x\";2;\"y\";1;\"v\";\"x\"];m[\"x\";0;\"y\";0;\"v\";\"o\"];m[\"x\";1;\"y\";0;\"v\";\"x\"];m[\"x\";0;\"y\";1;\"v\";\"o\"]]"
      serialize moves `shouldBe` str


  describe "Parse m-expression" $ do
    it "returns empty array if no moves are present" $ do
      parse "l[]" `shouldBe` []

    it "parses m-expr and returns an array of Move" $ do
      let str = "l[m[\"x\";  2; \"y\"; 1; \"v\";   \"x\"]; m[\"x\"; 0;  \"y\";   0; \"v\"; \"o\"]; m[\"x\";   1;  \"y\";   0; \"v\";   \"x\"]; m[\"x\";  0;  \"y\";  1; \"v\";   \"o\"]]"
      parse str `shouldBe` [ Move {x = 2, y = 1, player = 'x'}
                           , Move {x = 0, y = 0, player = 'o'}
                           , Move {x = 1, y = 0, player = 'x'}
                           , Move {x = 0, y = 1, player = 'o'} ]

    it "parses m-expr and returns an array of without fixed order of keys" $ do
      let str = "l[m[\"y\"; 1; \"v\";  \"x\";  \"x\"; 2  ]; m[\"x\"; 0 ; \"v\"; \"o\" ;   \"y\";   0]]"
      parse str `shouldBe` [Move {x = 2, y = 1, player = 'x'}, Move {x = 0, y = 0, player = 'o'}]