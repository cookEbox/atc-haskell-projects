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

import           Control.Monad                        (void, when, join)
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
import           Text.Parsec.Error                    (errorMessages)
import System.Environment (getArgs)

data Status = Complete
            | InProgress
            | ToDo
            deriving (Show, Eq, Read, Generic)

data Level = High
           | Medium
           | Low
           deriving (Show, Eq, Read, Generic, Ord)

instance IsString Level where
  fromString "High"   = High
  fromString "Medium" = Medium
  fromString "Low"    = Low

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
           | All (Maybe Status)
           | STS Status
           | PRI (Maybe Status)
           | Due (Maybe Status)
           deriving (Show, Eq)

data Commands = Add (Maybe Day)
              | View Group
              | Mark Id Status
              | Delete Id
              | Help
              | Priority Id Level
              | Edit Id (Maybe String)
              | Date Id (Maybe Day)
              | Exit
              deriving (Show, Eq)

databaseSetup :: IO ()
databaseSetup = runSqlite "tasks.db" $ do
  runMigration migrateAll

main :: IO ()
main = do
  databaseSetup
  args <- getArgs
  case null args of 
    True -> do
      putStrLn "Welcome to my TODO List Manager!"
      loop
    False -> if head args == "-h" || head args == "--help"
             then putStrLn helpMenu 
             else putStrLn $ head args <> " is not recognised\nTry -h or --help"

loop :: IO ()
loop = do
  putStr "Enter command: "
  hFlush stdout
  input <- getLine
  isLooping <- handleInput input
  when isLooping loop

handleInput :: String -> IO Bool
handleInput input = do
  case commands input of
    Left output  -> do putStrLn $ "You entered: " ++ show output ++ "\nTo see the help menu type: Help "
                       pure True
    Right Exit   -> do action Exit 
                       pure False
    Right output -> do action output
                       pure True

commands :: String -> Either ParseError Commands
commands cmd = parse ( try addParser
                   <|> try viewParser
                   <|> try markParser
                   <|> try dateParser
                   <|> try deleteParser
                   <|> try exitParser
                   <|> try priorityParser
                   <|> try editParser
                   <|> try helpParser
                     ) "test" lowerCMD
  where
    lowerCMD = toLower <$> cmd

action :: Commands -> IO ()
action Help = putStrLn helpMenu
action Exit = putStrLn "Goodbye!"
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
sorted (Just (Due _)) = uncurry (<>)
                      . foldr (\x (date,nodate) ->
                          if null (last x)
                          then (date, x : nodate)
                          else (x : date, nodate)) ([],[])
sorted (Just (PRI _)) = sortBy (compare `on` (!!1))
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

getDate :: Id -> IO (Maybe Day)
getDate id = runSqlite "tasks.db" $ do 
  task <- get (toSqlKey id :: Key Task)
  return $ task >>= taskDueDate

dayParse :: String -> Maybe Day
dayParse sDay = case parse dayParser "day" sDay of 
  Left err -> Nothing 
  Right day -> day

editDate :: Maybe Day -> IO (Maybe Day) 
editDate oldDay = runInputT defaultSettings $ do 
  minput <- getInputLineWithInitial "Edit the date: " (printDay oldDay, "")
  case minput of 
    Nothing -> pure Nothing
    Just day -> pure $ dayParse day
  where printDay Nothing = ""
        printDay (Just dy) = show dy

dateTask :: MonadIO m => Id -> Maybe Day -> Action m
dateTask id mDay@(Just _) = update (toSqlKey id :: Key Task) [TaskDueDate =. mDay]
dateTask id Nothing = do 
  newDate <- liftIO mDate 
  update (toSqlKey id :: Key Task) [TaskDueDate =. newDate] 
  where mDate = do 
          oldDate <- getDate id 
          case oldDate of 
            Nothing -> editDate Nothing 
            Just day -> editDate $ Just day 

priorityTask :: MonadIO m => Id -> Level -> Action m
priorityTask id pnum = update (toSqlKey id :: Key Task) [TaskPriority =. pnum]

markTask :: MonadIO m => Id -> Status -> Action m
markTask id status = update (toSqlKey id :: Key Task) [TaskStatus =. status]

deleteTask :: MonadIO m => Id -> Action m
deleteTask id = delete (toSqlKey id :: Key Task)

viewTask :: MonadIO m => Group -> Action m
viewTask (All mSts) = 
  case mSts of 
    Nothing -> do
      tasks <- selectList @Task [] []
      liftIO $ putStrLn $ pprintTask Nothing tasks
    Just sts -> do
      tasks <- selectList @Task [TaskStatus ==. sts] []
      liftIO $ putStrLn $ pprintTask Nothing tasks
viewTask (ID id) = do
  maybeTask <- get (toSqlKey id :: Key Task)
  case maybeTask of
      Just task -> liftIO $ print task
      Nothing   -> liftIO $ putStrLn $ "Task with ID " <> show id <> " not found."
viewTask (STS sts) = do
  task <- selectList @Task [TaskStatus ==. sts] []
  liftIO $ putStrLn $ pprintTask Nothing task
viewTask grp@(Due mSts) = 
  case mSts of 
    Nothing -> do
      tasks <- selectList @Task [] [Asc TaskDueDate]
      liftIO $ putStrLn $ pprintTask (Just grp) tasks
    Just sts -> do
      tasks <- selectList @Task [TaskStatus ==. sts] []
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

exitParser :: Parse Commands 
exitParser = do 
  void $ string "exit"
  pure Exit

stringToDay :: String -> Maybe Day
stringToDay = parseTimeM True defaultTimeLocale "%Y-%m-%d"

dayParser :: Parse (Maybe Day)
dayParser = do
  void $ optional space
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
  day <- optionMaybe dayParser
  pure $ Date (read id) (join day)

helpParser :: Parse Commands
helpParser = do
  void $ string "help"
  pure Help

viewParser :: Parse Commands
viewParser = do
  void $ string "view"
  void space
  View <$> ( try vStatusParser
         <|> try allParser
         <|> try idParser
         <|> try dueParser
         <|> try priParse
           )

priParse :: Parse Group
priParse = do
  pri <- string "priority"
  void $ optional space
  mGrp <- optionMaybe statusParser
  pure $ PRI mGrp

dueParser :: Parse Group
dueParser = do
  due <- string "due"
  mspace <- optionMaybe space
  case mspace of 
    Nothing -> pure $ Due Nothing
    Just _ -> Due . Just <$> statusParser

idParser :: Parse Group
idParser = do
  id <- many1 digit
  pure $ ID (read id)

allParser :: Parse Group
allParser = do
  void $ string "all"
  mspace <- optionMaybe space
  case mspace of 
    Nothing -> pure $ All Nothing
    Just _ -> All . Just <$> statusParser

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
  
helpMenu :: String
helpMenu = "This is the help menu \
         \\n \
         \\nUsage: \
         \\n  Interactive: (CASE INSENSITIVE) \
         \\n    Commands: \
         \\n      Add                       Will ask for task description and then creates the task \
         \\n      Date <id>                 Interactive task date editor \
         \\n      Date <id> <date>          Adds / updates due date on task \
         \\n      Delete <id>               Deletes the task with that id \
         \\n      Edit <id>                 Interactive task desctiption editor \
         \\n      Edit <id> <description>   Pass a new description to task with this id (lowercase only) \
         \\n      Exit                      Exits the program \
         \\n      Help                      Prints this menu \
         \\n      Mark <id> <status>        Marks the task with that is with that status \
         \\n      Priority <id> <priority>  Changes the task's priority to the given priortiy \
         \\n      View                          \
         \\n        View All                Prints all tasks \
         \\n        View All <status>       Prints all tasks with that status \
         \\n        View Due                Prints all tasks organised by due date \
         \\n        View Due <status>       Prints all tasks organised by due date with that status \
         \\n    Inputs: \
         \\n      Id                        Integer \
         \\n      Date                      yyyy-mm-dd \
         \\n      Status                    ToDo / InProgress / Complete \
         \\n      Priority                  Low / Medium / High \
         \\n"
