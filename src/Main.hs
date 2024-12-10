module Main where

import System.IO (hFlush, stdout)
import Control.Monad (when, void)
import Data.Time.Clock
import Text.Parsec
import Data.Functor.Identity (Identity)
import Data.Char (toLower, isDigit)

-- newtype Error = Error String

type Id = Int

type Parse a = ParsecT String () Identity a

data OOA = ID Id | All deriving (Show, Eq)

data Commands = Add | View OOA | Mark Status Id | Delete Id | Help deriving (Show, Eq)

data Status = Complete | InProgress | ToDo deriving (Show, Eq)

data Task = Task 
  { description :: String
  , id          :: Id
  , status      :: Status
  , dueDate     :: UTCTime
  } deriving (Show, Eq)

main :: IO ()
main = do
  putStrLn "Welcome to my TODO List Manager!"
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
  case commands input of 
    Left output  -> putStrLn $ "You entered: " ++ show output
    Right output -> putStrLn $ "You entered: " ++ show output
  pure True

commands :: String -> Either ParseError Commands
commands cmd = parse ( addParser 
                   <|> viewParser 
                   <|> markParser 
                   <|> deleteParser 
                   <|> helpParser 
                     ) "test" lowerCMD
  where 
    lowerCMD = toLower <$> cmd

addParser :: Parse Commands 
addParser = do 
  void $ string "add"
  pure Add

helpParser :: Parse Commands 
helpParser = do 
  void $ string "help"
  pure Help
   
viewParser :: Parse Commands 
viewParser = do 
  void $ string "view"
  void space 
  id <- many1 digit <|> string "all" 
  case all isDigit id of 
    True  -> pure $ View (ID $ read id)
    False -> pure $ View All

deleteParser :: Parse Commands 
deleteParser = do 
  void $ string "delete"
  void space 
  id <- many1 digit 
  pure $ Delete (read id)

markParser :: Parse Commands 
markParser = do 
  void $ string "mark"
  void space 
  sts <- statusParser 
  void space 
  id <- many1 digit 
  pure $ Mark sts (read id)

completeParser :: Parse Status 
completeParser = do 
  void $ string "complete"
  pure Complete

inProgressParser :: Parse Status 
inProgressParser = do 
  void $ string "inprogress"
  pure InProgress

toDoParser :: Parse Status 
toDoParser = do 
  void $ string "todo"
  pure ToDo

statusParser :: Parse Status 
statusParser = do toDoParser 
              <|> inProgressParser 
              <|> completeParser
