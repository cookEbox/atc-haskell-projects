module Main where

import System.IO (hFlush, stdout)
import Control.Monad (when)

newtype PlayerX = PlayerX Int
newtype PlayerO = PlayerO Int

data Score = Score PlayerX PlayerO

data Coord = X | O | B deriving Eq

instance Show Coord where 
  show X = "X"
  show O = "O"
  show B = " "

type Board = [[Coord]]

data BoardState = BoardState 
  { score :: Score 
  , board :: Board
  }

blankBoard :: Board
blankBoard = [[B,B,B],[B,B,B],[B,B,B]]

printBoard :: Board -> IO ()
printBoard = putStrLn . (<>) "\n" . unlines . addLines . fmap printRow 
  where 
    printRow [x,y,z] = " " <> show x <> " | " <> show y <> " | " <> show z 
    addLines [f,s,t] = [f, hor, s, hor, t ]
    hor = "--- --- ---"

main :: IO ()
main = do
  putStrLn "Welcome to Tic Tac Toe!"
  loop

loop :: IO ()
loop = do
  putStr "Enter command: "
  hFlush stdout
  input <- getLine
  isLooping <- handleInput input
  when isLooping loop

handleInput :: String -> IO Bool
handleInput "exit" = do
  putStrLn "Goodbye!"
  pure False
handleInput input = do
  putStrLn $ "You entered: " ++ input
  pure True
