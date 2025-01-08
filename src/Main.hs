module Main where

import System.IO (hFlush, stdout)
import Control.Monad (when)
import Data.IORef (newIORef, IORef, readIORef, writeIORef)
import System.Process (callCommand)
import Data.List.Split (chunksOf)

data ID = A | B deriving Eq

instance Show ID where 
  show A = "1"
  show B = "2"

data Player = Player 
  { _id   :: ID
  , name  :: String
  , score :: Int
  , token :: Token
  }

instance Show Player where 
  show p  = "Player "
         <> show (_id p)
         <> "\n    Name: " 
         <> name p 
         <> "\n    Score: " 
         <> show (score p) 
         <> "\n    Token: " 
         <> show (token p)

data Game = Game Player Player 

instance Show Game where 
  show (Game p1 p2) = show p1 <> "\n" <> show p2

data Coord = Turn Token | Blank deriving Eq

instance Show Coord where 
  show (Turn token) = show token
  show Blank        = " "

data Token = X | O deriving Eq

instance Show Token where 
  show X = "X"
  show O = "O"

type Board = [[Coord]]

data GameState = GameState 
  { game :: Game 
  , board :: Board
  , go    :: Token
  } deriving Show

blankBoard :: Board
blankBoard = [[Blank,Blank,Blank],[Blank,Blank,Blank],[Blank,Blank,Blank]]

printGame :: Board -> IO ()
printGame = putStrLn . (<>) "\n" . unlines . addLines . fmap printRow . numbered 
  where 
    printRow [x,y,z] = " " <> x <> " | " <> y <> " | " <> z
    addLines [f,s,t] = [f, hor, s, hor, t ]
    numbered         = zipWith2 ifB [["1","2","3"],["4","5","6"],["7","8","9"]]
    ifB n b          = if b == Blank then n else show b
    hor              = "--- --- ---"
    zipWith2         = zipWith . zipWith

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
            $ updateGameState blankBoard (player1, player2) (0,0) (X,O) X
          newBoard (player1, player2) False 
            = newIORef 
            $ updateGameState blankBoard (player1, player2) (0,0) (O,X) X

updateGameState :: Board -> (String, String) -> (Int, Int) -> (Token,Token) -> Token -> GameState
updateGameState _board (_name1, _name2) (_score1, _score2) (_token1, _token2) _go = 
  GameState 
    { game = Game 
    ( Player { _id = A, name = _name1, score = _score1, token = _token1 } ) 
    ( Player { _id = B, name = _name2, score = _score2, token = _token2 } )
    , board = _board 
    , go = _go
    }

loop :: IORef GameState -> IO ()
loop state = do
  current <- readIORef state
  callCommand "clear" 
  print $ game current
  printGame $ board current
  putStr $ playerToGoName (go current) (game current) <> " Enter command: "
  hFlush stdout
  input <- getLine
  isLooping <- handleInput state input
  when isLooping $ loop state
  where 
    playerToGoName _go = name 
                       . head 
                       . filter (\x -> token x == _go) 
                       . playersToGo
    playersToGo (Game p1 p2) = [p1,p2]

handleInput :: IORef GameState -> String -> IO Bool
handleInput _ "exit" = do
  putStrLn "Goodbye!"
  pure False
handleInput state input = 
  if input `elem` ["1", "2", "3", "4", "5", "6", "7", "8", "9"] 
  then do 
    current <- readIORef state
    let coord         = read input :: Int
        _go           = go current
        _board        = board current
        concatBoard   = concat _board
        replace n b g = take (n-1) b <> [g] <> drop n b
        switch O      = X
        switch X      = O
    if concatBoard !! (coord -1) == Blank 
    then do writeIORef state $ current { board = chunksOf 3 (replace coord concatBoard (Turn _go)), go = switch _go }
            return True
    else return True

  else do
    putStrLn $ "You entered: " ++ input
    pure True
