{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Main where

import           Control.Monad                        (void, when)
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Trans.Reader           (ReaderT)
import           Data.Char                            (isDigit, toLower)
import           Data.Function                        (on)
import           Data.Functor.Identity                (Identity)
import           Data.Kind                            (Type)
import           Data.List                            (sortBy)
import           Data.String                          (IsString, fromString)
import           Data.Text                            (Text, pack, unpack)
import           Data.Time.Clock
import           Database.Persist                     hiding (Add)
import           Database.Persist.Sql                 (PersistField,
                                                       PersistFieldSql,
                                                       SqlType (SqlString),
                                                       runMigration, sqlType,
                                                       toSqlKey)
import           Database.Persist.SqlBackend.Internal (SqlBackend)
import           Database.Persist.Sqlite              (runSqlite)
import           Database.Persist.TH
import           GHC.Generics                         (Generic)
import           GHC.Int                              (Int64)
import           System.IO                            (hFlush, stdout)
import           Text.Parsec

data Status = Complete | InProgress | ToDo deriving (Show, Eq, Read, Generic)

instance IsString Status where
  fromString "Complete"   = Complete
  fromString "InProgress" = InProgress
  fromString "ToDo"       = ToDo

derivePersistField "Status"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Task
    description String
    status      Status
    dueDate     UTCTime
    priority    Int64
    deriving Show Eq
|]

type Id = Int64

type Parse a = ParsecT String () Identity a

data OOA = ID Id | All | O Status deriving (Show, Eq)

data Commands = Add | View OOA | Mark Id Status | Delete Id | Help | Priority Id Int64 deriving (Show, Eq)

databaseSetup :: IO ()
databaseSetup = runSqlite "tasks.db" $ do
  runMigration migrateAll

main :: IO ()
main = do
  databaseSetup
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
    Right output -> action output
  pure True

commands :: String -> Either ParseError Commands
commands cmd = parse ( addParser
                   <|> viewParser
                   <|> markParser
                   <|> deleteParser
                   <|> priorityParser
                   <|> helpParser
                     ) "test" lowerCMD
  where
    lowerCMD = toLower <$> cmd

action :: Commands -> IO ()
action Help = putStrLn helpMenu
action cmd =
  runSqlite "tasks.db" $
    case cmd of
      View ooa         -> viewTask ooa
      Delete id        -> deleteTask id
      Mark id status   -> markTask id status
      Priority id pnum -> priorityTask id pnum
      Add              -> addTask

helpMenu :: String
helpMenu = "This is the help menu"

priorityTask :: MonadIO m => Id -> Int64 -> ReaderT SqlBackend m ()
priorityTask id pnum = update (toSqlKey id :: Key Task) [TaskPriority =. pnum]

markTask :: MonadIO m => Id -> Status -> ReaderT SqlBackend m ()
markTask id status = update (toSqlKey id :: Key Task) [TaskStatus =. status]

deleteTask :: MonadIO m => Id -> ReaderT SqlBackend m ()
deleteTask id = delete (toSqlKey id :: Key Task)

viewTask :: MonadIO m => OOA -> ReaderT SqlBackend m ()
viewTask All = do
  tasks <- selectList @Task [] []
  liftIO $ putStrLn $ pprintTask tasks
viewTask (ID id) = do
  maybeTask <- get (toSqlKey id :: Key Task)
  case maybeTask of
      Just task -> liftIO $ print task
      Nothing   -> liftIO $ putStrLn $ "Task with ID " <> show id <> " not found."
viewTask (O sts) = do
  task <- selectList @Task [TaskStatus ==. sts] []
  liftIO $ putStrLn $ pprintTask task

pprintTask :: [ Entity Task ] -> String
pprintTask = unlines
  . fmap unwords
  . sortBy (compare `on` head)
  . fmap (( \x -> show (taskPriority x)
                : show (taskDescription x)
                : show (taskStatus x)
                : [show (taskDueDate x)])
                . entityVal)

addTask :: MonadIO m => ReaderT SqlBackend m ()
addTask = do
  liftIO $ putStr "Enter task: "
  liftIO $ hFlush stdout
  input <- liftIO getLine
  currentTime <- liftIO getCurrentTime
  let newTask = Task input "ToDo" currentTime 0
  taskId <- insert newTask
  liftIO $ putStrLn $ "Inserted task with ID: " ++ show taskId

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
  View <$> (vStatusParser <|> allParser <|> idParser)

idParser :: Parse OOA
idParser = do
  id <- many1 digit
  pure $ ID (read id)

allParser :: Parse OOA
allParser = do
  void $ string "all"
  pure All

vStatusParser :: Parse OOA
vStatusParser = O <$> statusParser

deleteParser :: Parse Commands
deleteParser = do
  void $ string "delete"
  void space
  id <- many1 digit
  pure $ Delete (read id)

priorityParser :: Parse Commands
priorityParser = do
  void $ string "priority"
  void space
  id <- many1 digit
  void space
  pnum <- many1 digit
  pure $ Priority (read id) (read pnum)

markParser :: Parse Commands
markParser = do
  void $ string "mark"
  void space
  id <- many1 digit
  void space
  sts <- statusParser
  pure $ Mark (read id) sts

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
