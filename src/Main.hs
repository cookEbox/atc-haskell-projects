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
import           Data.Char                            (toLower)
import           Data.Function                        (on)
import           Data.Functor.Identity                (Identity)
import           Data.Kind                            (Type)
import           Data.List                            (sortBy, transpose)
import           Data.Maybe                           (fromJust, fromMaybe,
                                                       isNothing)
import           Data.String                          (IsString, fromString)
import           Data.Text                            (Text, pack, unpack)
import           Data.Time                            (Day)
import           Data.Time.Format                     (defaultTimeLocale,
                                                       parseTimeM)
import           Database.Persist                     hiding (Add, count)
import           Database.Persist.Sql                 (PersistField,
                                                       PersistFieldSql,
                                                       SqlType (SqlString),
                                                       fromSqlKey, runMigration,
                                                       sqlType, toSqlKey)
import           Database.Persist.SqlBackend.Internal (SqlBackend)
import           Database.Persist.Sqlite              (runSqlite)
import           Database.Persist.TH
import           GHC.Generics                         (Generic)
import           GHC.Int                              (Int64)
import           System.Console.Haskeline
import           System.IO                            (hFlush, stdout)
import           Text.Parsec

data Status = Complete
            | InProgress
            | ToDo
            deriving (Show, Eq, Read, Generic)

data Level = High
           | Medium
           | Low
           deriving (Show, Eq, Read, Generic, Ord)

-- instance Ord Level where
--   compare High _ = GT
--   compare _ Low  = GT

instance IsString Level where
  fromString "High"   = High
  fromString "Medium" = Medium
  fromString "Low"    = Low
  fromString "high"   = High
  fromString "medium" = Medium
  fromString "low"    = Low

instance IsString Status where
  fromString "Complete"   = Complete
  fromString "InProgress" = InProgress
  fromString "ToDo"       = ToDo

derivePersistField "Status"

derivePersistField "Level"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Task
    description String
    status      Status
    dueDate     (Maybe Day)
    priority    Level
    deriving Show Eq
|]

type Id = Int64

type Parse a = ParsecT String () Identity a

type Action m = ReaderT SqlBackend m ()

data Group = ID Id
           | All
           | STS Status
           | PRI
           | Due
           deriving (Show, Eq)

data Commands = Add (Maybe Day)
              | View Group
              | Mark Id Status
              | Delete Id
              | Help
              | Priority Id Level
              | Edit Id (Maybe String)
              | Date Id (Maybe Day)
              deriving (Show, Eq)

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
                   <|> dateParser
                   <|> deleteParser
                   <|> priorityParser
                   <|> editParser
                   <|> helpParser
                     ) "test" lowerCMD
  where
    lowerCMD = toLower <$> cmd

action :: Commands -> IO ()
action Help = putStrLn helpMenu
action cmd =
  runSqlite "tasks.db" $
    case cmd of
      View group       -> viewTask group
      Delete id        -> deleteTask id
      Mark id status   -> markTask id status
      Priority id pnum -> priorityTask id pnum
      Add mday         -> addTask mday
      Date id day      -> dateTask id day
      Edit id mDes     -> editTask id mDes

helpMenu :: String
helpMenu = "This is the help menu"

padColumns :: [[String]] -> [[String]]
padColumns rows =
    let transposed = transpose rows
        maxLengths = maximum . map length <$> transposed
    in transpose $ zipWith (map . padRight) maxLengths transposed

padRight :: Int -> String -> String
padRight n str = str ++ replicate (n - length str) ' '

addHeadings :: [[String]] -> [[String]]
addHeadings = (:) ["ID", "Priority", "Description", "Status", "Due Date"]

addLine :: [[String]] -> [[String]]
addLine (title : body) = title : line : body
  where widths = length <$> title
        line   = (`replicate` '-') <$> widths

addSpacer :: [[String]] -> [[String]]
addSpacer = fmap $ insertBetween " | "

insertBetween :: a -> [a] -> [a]
insertBetween sep = foldr (\x acc -> x : sep : acc) [] . initLast
  where
    initLast [] = []
    initLast xs = init xs ++ [last xs]

sorted :: Maybe Group -> [[String]] -> [[String]]
sorted (Just Due) = uncurry (<>)
                  . foldr (\x (date,nodate) ->
                      if null (last x)
                      then (date, x : nodate)
                      else (x : date, nodate)) ([],[])
sorted (Just PRI) = sortBy (compare `on` (!!1))
sorted _ = id

pprintTask :: Maybe Group -> [Entity Task] -> String
pprintTask mg = unlines
              . fmap unwords
              . addLine
              . addSpacer
              . padColumns
              . addHeadings
              . sorted mg
              . fmap (\entity ->
                        let task = entityVal entity
                            key  = show (fromSqlKey $ entityKey entity)
                        in  key
                          : show (taskPriority task)
                          : show (taskDescription task)
                          : show (taskStatus task)
                          : [prettyDate task])
    where prettyDate task | isNothing $ taskDueDate task = ""
                          | otherwise = show $ fromJust (taskDueDate task)

getDesc :: Id -> IO (Maybe String)
getDesc id = runSqlite "tasks.db" $ do 
  des <- get (toSqlKey id :: Key Task)
  return $ taskDescription <$> des

editDesc :: String -> IO String
editDesc oldDes = runInputT defaultSettings $ do 
  minput <- getInputLineWithInitial "Edit the text: " (oldDes, "")
  case minput of 
    Nothing -> return "" 
    Just des -> return des 
  
editTask :: MonadIO m => Id -> Maybe String -> Action m
editTask id (Just des) = update (toSqlKey id :: Key Task) [TaskDescription =. des]
editTask id Nothing = do 
  newDes <- liftIO mDes
  update (toSqlKey id :: Key Task) [TaskDescription =. newDes]
  where mDes = do 
          oldDes <- getDesc id 
          case oldDes of 
            Nothing -> pure "" 
            Just des -> editDesc des

dateTask :: MonadIO m => Id -> Maybe Day -> Action m
dateTask id day = update (toSqlKey id :: Key Task) [TaskDueDate =. day]

priorityTask :: MonadIO m => Id -> Level -> Action m
priorityTask id pnum = update (toSqlKey id :: Key Task) [TaskPriority =. pnum]

markTask :: MonadIO m => Id -> Status -> Action m
markTask id status = update (toSqlKey id :: Key Task) [TaskStatus =. status]

deleteTask :: MonadIO m => Id -> Action m
deleteTask id = delete (toSqlKey id :: Key Task)

viewTask :: MonadIO m => Group -> Action m
viewTask All = do
  tasks <- selectList @Task [] []
  liftIO $ putStrLn $ pprintTask Nothing tasks
viewTask (ID id) = do
  maybeTask <- get (toSqlKey id :: Key Task)
  case maybeTask of
      Just task -> liftIO $ print task
      Nothing   -> liftIO $ putStrLn $ "Task with ID " <> show id <> " not found."
viewTask (STS sts) = do
  task <- selectList @Task [TaskStatus ==. sts] []
  liftIO $ putStrLn $ pprintTask Nothing task
viewTask grp = do
  tasks <- selectList @Task [] [Asc TaskDueDate]
  liftIO $ putStrLn $ pprintTask (Just grp) tasks

addTask :: MonadIO m => Maybe Day -> Action m
addTask mday = do
  liftIO $ putStr "Enter task: "
  liftIO $ hFlush stdout
  input <- liftIO getLine
  let newTask = Task input "ToDo" mday Low
  taskId <- insert newTask
  liftIO $ putStrLn $ "Inserted task with ID: " ++ show taskId

addParser :: Parse Commands
addParser = do
  void $ string "add"
  mday <- dayParser <|> noDayParser
  pure $ Add mday

noDayParser :: Parse (Maybe Day)
noDayParser = do
  void eof
  pure Nothing

stringToDay :: String -> Maybe Day
stringToDay = parseTimeM True defaultTimeLocale "%Y-%m-%d"

dayParser :: Parse (Maybe Day)
dayParser = do
  void space
  year <- count 4 digit
  void $ char '-'
  month <- count 2 digit
  void $ char '-'
  day <- count 2 digit
  pure $ stringToDay $ year <> "-" <> month <> "-" <> day

dateParser :: Parse Commands
dateParser = do
  void $ string "date"
  void space
  id <- many1 digit
  day <- dayParser
  pure $ Date (read id) day

helpParser :: Parse Commands
helpParser = do
  void $ string "help"
  pure Help

viewParser :: Parse Commands
viewParser = do
  void $ string "view"
  void space
  View <$> ( vStatusParser
         <|> allParser
         <|> idParser
         <|> dueParser
         <|> priParse
           )

priParse :: Parse Group
priParse = do
  pri <- string "priority"
  pure PRI

dueParser :: Parse Group
dueParser = do
  due <- string "due"
  pure Due

idParser :: Parse Group
idParser = do
  id <- many1 digit
  pure $ ID (read id)

allParser :: Parse Group
allParser = do
  void $ string "all"
  pure All

vStatusParser :: Parse Group
vStatusParser = STS <$> statusParser

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
  level <- levelParser
  pure $ Priority (read id) level

levelParser :: Parse Level
levelParser = highParser
          <|> mediumParser
          <|> lowParser

highParser :: Parse Level
highParser = do
  high <- string "high"
  pure High

mediumParser :: Parse Level
mediumParser = do
  high <- string "medium"
  pure Medium

lowParser :: Parse Level
lowParser = do
  high <- string "low"
  pure Low

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

editParser :: Parse Commands 
editParser = do 
  void $ string "edit" 
  void space 
  id <- many1 digit 
  mDes <- optionMaybe descParser 
  pure $ Edit (read id) mDes

descParser :: Parse String
descParser = do 
  void space
  many1 anyChar
  
  
  
