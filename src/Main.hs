module Main where

import System.IO (hFlush, stdout)
import Control.Monad (when)
import Data.IORef (newIORef, IORef, readIORef)
import System.Process (callCommand)

data Player1 = Player1 
  { name1  :: String
  , score1 :: Int
  , token1 :: Token
  }

data Player2 = Player2 
  { name2  :: String
  , score2 :: Int
  , token2 :: Token
  }

instance Show Player1 where 
  show p  = "Player 1\n    Name: " 
         <> name1 p 
         <> "\n    Score: " 
         <> show (score1 p) 
         <> "\n    Token: " 
         <> show (token1 p)

instance Show Player2 where 
  show p  = "Player 2\n    Name: " 
         <> name2 p 
         <> "\n    Score: " 
         <> show (score2 p) 
         <> "\n    Token: " 
         <> show (token2 p)

data Score = Score Player1 Player2 

instance Show Score where 
  show (Score p1 p2) = show p1 <> "\n" <> show p2

data Coord = Turn Token | B deriving Eq

instance Show Coord where 
  show (Turn token) = show token
  show B            = " "

data Token = X | O deriving Eq

instance Show Token where 
  show X = "X"
  show O = "O"

type Board = [[Coord]]

data GameState = GameState 
  { score :: Score 
  , board :: Board
  , go    :: Token
  } deriving Show

blankBoard :: Board
blankBoard = [[B,B,B],[B,B,B],[B,B,B]]

printGame :: Board -> IO ()
printGame = putStrLn . (<>) "\n" . unlines . addLines . fmap printRow . numbered 
  where 
    printRow [x,y,z] = " " <> x <> " | " <> y <> " | " <> z
    addLines [f,s,t] = [f, hor, s, hor, t ]
    numbered         = zipWith2 ifB [["1","2","3"],["4","5","6"],["7","8","9"]]
    ifB n b          = if b == B then n else show b
    hor              = "--- --- ---"
    zipWith2         = zipWith . zipWith

printPlayers :: Score -> IO () 
printPlayers = print

main :: IO ()
main = do
  putStrLn "Welcome to Tic Tac Toe!"
  initial <- playerSetup
  loop initial

playerSetup :: IO (IORef GameState)
playerSetup = do
  putStr "Enter First Players Name: "
  hFlush stdout
  player1 <- getLine
  putStr "Enter Second Players Name: "
  hFlush stdout
  player2 <- getLine
  putStr "Choose a player to go first (X): "
  hFlush stdout
  firstPlayer <- getLine 
  case firstPlayer of 
    "1"              -> newBoard (player1, player2) True
    "2"              -> newBoard (player1, player2) False
    p | p == player1 -> newBoard (player1, player2) True
    p | p == player2 -> newBoard (player1, player2) False
    _                -> playerSetup
    where newBoard (player1, player2) True  
            = newIORef 
            $ updateGameState (player1, player2) (0,0) (X,O) X
          newBoard (player1, player2) False 
            = newIORef 
            $ updateGameState (player1, player2) (0,0) (O,X) X

updateGameState :: (String, String) -> (Int, Int) -> (Token,Token) -> Token -> GameState
updateGameState (_name1, _name2) (_score1, _score2) (_token1, _token2) _go = 
  GameState 
    { score = Score 
    ( Player1 { name1 = _name1, score1 = _score1, token1 = _token1 } ) 
    ( Player2 { name2 = _name2, score2 = _score2, token2 = _token2 } )
    , board = blankBoard 
    , go = _go
    }

loop :: IORef GameState -> IO ()
loop state = do
  current <- readIORef state
  callCommand "clear" 
  printPlayers $ score current
  printGame $ board current
  putStr $ playerToGoName current <> " Enter command: "
  hFlush stdout
  input <- getLine
  isLooping <- handleInput input
  when isLooping $ loop state
  where 
    playerToGoName gamestate = playerToGo (score gamestate) (go gamestate)
    playerToGo (Score p1 p2) _go = fst 
                                 . head 
                                 . filter ((_go ==) . snd) 
                                 $ [(name1 p1, token1 p1), (name2 p2, token2 p2)]

handleInput :: String -> IO Bool
handleInput "exit" = do
  putStrLn "Goodbye!"
  pure False
handleInput input = do
  putStrLn $ "You entered: " ++ input
  pure True
