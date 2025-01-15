module Main where

import           Control.Monad       (join, when)
import           Control.Monad.State (StateT, evalStateT, get, gets, liftIO,
                                      put)
import           Data.Bifunctor      (Bifunctor (bimap))
import           Data.Char           (toLower)
import           Data.IORef          (IORef, newIORef, readIORef, writeIORef)
import           Data.List           (transpose)
import           Data.List.Split     (chunksOf)
import           System.IO           (hFlush, stdout)
import           System.Process      (callCommand)
import           System.Random       (randomRIO)
import Data.Maybe (mapMaybe)

type Msg = String

bot :: String
bot = "BOT"

data GameType = AI | PvP deriving Eq

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
         <> "\n-------"
         <> "\nName: "
         <> name p
         <> "\nScore: "
         <> show (score p)
         <> "\nToken: "
         <> show (token p)

data Game = Game Player Player

instance Show Game where
  show (Game p1 p2) = unlines $ zipWith (<>) (padColumns . lines $ show p1) (lines $ show p2)

padColumns :: [String] -> [String]
padColumns rows = padRight (maxLengths + 3) <$> rows
    where
      maxLengths = maximum . map length $ rows

padRight :: Int -> String -> String
padRight n str = str ++ replicate (n - length str) ' '

data Coord = Turn Token | Blank deriving Eq

instance Show Coord where
  show (Turn token) = show token
  show Blank        = " "

data Token = X | O deriving Eq

instance Show Token where
  show X = "X"
  show O = "O"

switch :: Token -> Token
switch O = X
switch X = O

type Board = [[Coord]]

data GameState = GameState
  { game  :: Game
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
  gameType <- playerOrAI
  case gameType of
    PvP -> do initial <- playerSetup
              evalStateT (playerLoop "") initial
    AI  -> do initial <- aiSetup
              evalStateT (aiLoop "") initial

playerOrAI :: IO GameType
playerOrAI = do
  putStr "Please choose between AI (AI) or Player vs Player (PvP): "
  hFlush stdout
  input <- getLine
  case toLower <$> input of
    "ai" -> return AI
    "pvp" -> return PvP
    _ -> do putStrLn "Incorrect selection!"
            playerOrAI

aiSetup :: IO GameState
aiSetup = do
  putStr "Enter Player's Name: "
  hFlush stdout
  player1 <- getLine
  let player2 = bot
  putStrLn $ "AI opponenet is called: " <> player2
  putStr "Choose a player to go first (X): "
  hFlush stdout
  firstPlayer <- getLine
  case firstPlayer of
    "1"              -> newBoard (player1, player2) True
    "2"              -> newBoard (player1, player2) False
    p | p == player1 -> newBoard (player1, player2) True
    p | p == player2 -> newBoard (player1, player2) False
    _                -> aiSetup

playerSetup :: IO GameState
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

newBoard (player1, player2) True
  = return $ updateGameState blankBoard (player1, player2) (0,0) (X,O) X
newBoard (player1, player2) False
  = return $ updateGameState blankBoard (player1, player2) (0,0) (O,X) X

updateGameState :: Board -> (String, String) -> (Int, Int) -> (Token,Token) -> Token -> GameState
updateGameState _board (_name1, _name2) (_score1, _score2) (_token1, _token2) _go =
  GameState
    { game = Game
    ( Player { _id = A, name = _name1, score = _score1, token = _token1 } )
    ( Player { _id = B, name = _name2, score = _score2, token = _token2 } )
    , board = _board
    , go = _go
    }

aiLoop :: String -> StateT GameState IO ()
aiLoop msg = do
  current <- get
  let currentPlayer = playerToGoName (go current) (game current)
  case currentPlayer of
    p | p == bot -> do stillPlaying <- aiAlgo 
                       case stillPlaying of 
                        Left _ -> aiLoop "The AI is selecting the wrong coordinate...." 
                        Right bl -> when bl $ aiLoop ""
    _ -> do stillPlaying <- playerGo msg
            case stillPlaying of
              Left err -> aiLoop err
              Right bl -> when bl $ aiLoop ""

randomSelect :: [Int] -> IO Int
randomSelect xs = (!!) xs <$> randomRIO (0, length xs - 1)

aiAlgo :: StateT GameState IO (Either Msg Bool)
aiAlgo = do
  current <- get
  let (Game player bot) = game current
      ptoken = token player
      btoken = token bot
      _board = board current
      playerWin = randomSelect <$> checkForWinMve ptoken _board
      botWin = randomSelect <$> checkForWinMve btoken _board
      noWin = randomSelect $ boardCoordsLeft _board
  case botWin of 
    Just ioInt -> do pos <- liftIO ioInt 
                     updateBoard pos
    Nothing -> case playerWin of 
                Just ioInt -> do pos <- liftIO ioInt 
                                 updateBoard pos 
                Nothing -> do pos <- liftIO noWin 
                              updateBoard pos

boardCoordsLeft :: Board -> [Int] 
boardCoordsLeft = fmap fst . filtered . coordPos
  where
    positions = [[1,2,3],[4,5,6],[7,8,9]] :: [[Int]]
    coordPos  = zipWith zip positions 
    filtered  = concatMap (filter (\ x -> snd x == Blank))
-- Needs to check if there is a win for bot and play win
-- Needs to check if there is a win for player and block
-- Needs to randomly pick an available coord

playerGo :: String -> StateT GameState IO (Either Msg Bool)
playerGo msg = do
  current <- get
  let currentPlayer = go current
  liftIO $ do
    callCommand "clear"
    print $ game current
    printGame $ board current
    putStrLn msg
    putStr $ playerToGoName (go current) (game current) <> " Enter command: "
    hFlush stdout
  input <- liftIO getLine
  stillPlaying <- handleInput (toLower <$> input) -- handleInput needs to be before checkGame
  stillGoing <- checkGame
  updateGame stillGoing currentPlayer
  return stillPlaying

playerLoop :: String -> StateT GameState IO ()
playerLoop msg = do
  stillPlaying <- playerGo msg
  case stillPlaying of
    Left err -> playerLoop err
    Right bl -> when bl $ playerLoop ""

playerToGoName :: Token -> Game -> String
playerToGoName _go = name
                   . head
                   . filter (\x -> token x == _go)
                   . listOfPlayers
  where
    listOfPlayers (Game p1 p2) = [p1,p2]

updateGame :: Bool -> Token -> StateT GameState IO ()
updateGame False _ = return ()
updateGame True player = do
  current <- get
  let playersTuple (Game p1 p2) = (p1, p2)
      _game  = game current
      names  = join bimap name $ playersTuple _game
      scores = addToPlayersScore player $ playersTuple _game
      tokens = join bimap switch $ join bimap token $ playersTuple _game
  put $ updateGameState blankBoard names scores tokens (switch $ go current)

addToPlayersScore :: Token -> (Player, Player) -> (Int, Int)
addToPlayersScore t (p1, p2) | token p1 == t = ((+1) $ score p1, score p2)
                             | otherwise     = (score p1, (+1) $ score p2)

checkGame :: StateT GameState IO Bool
checkGame = gets $ checkRows . allCombinations . board

checkRows :: [[Coord]] -> Bool
checkRows   = any ( \x -> all (== Turn O) x || all (== Turn X) x )

checkForWinMve :: Token -> [[Coord]] -> Maybe [Int]
checkForWinMve token board = case matched2 of
  []   -> Nothing
  ints -> Just ints
  where
    positions = [[1,2,3],[4,5,6],[7,8,9]]
    coordPos  = zipWith zip positions board
    matched2  = mapMaybe (match2 token) (coordPos <> diagonals coordPos)

match2 :: Token -> [(Int, Coord)] -> Maybe Int
match2 token [a, b, c] | Turn token == snd a && Turn token == snd b = Just $ fst c
                       | Turn token == snd a && Turn token == snd c = Just $ fst b
                       | Turn token == snd b && Turn token == snd c = Just $ fst a
                       | otherwise = Nothing

allCombinations b = b <> transpose b <> diagonals b
diagonals b =
  [
  [ topLeft b, middle b, bottomRight b ]
  ,
  [ topRight b, middle b, bottomLeft b ]
  ]
  where
    topLeft     = (!!0) . (!!0)
    middle      = (!!1) . (!!1)
    bottomRight = (!!2) . (!!2)
    topRight    = (!!0) . (!!2)
    bottomLeft  = (!!2) . (!!0)

handleInput :: String -> StateT GameState IO (Either Msg Bool)
handleInput "exit" = return $ Right False
handleInput "help" = return $ Left help
handleInput input  =
  if input `elem` ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
  then updateBoard (read input :: Int)
  else return $ Left (input <> " -- is not a valid command\n" <> help)

updateBoard :: Int -> StateT GameState IO (Either Msg Bool)
updateBoard coord = do 
    current <- get
    let _go           = go current
        _board        = board current
        joinBoard     = join _board
        replace n b g = take (n-1) b <> [g] <> drop n b
    if joinBoard !! (coord -1) == Blank
    then do put current { board = chunksOf 3 (replace coord joinBoard (Turn _go))
                        , go    = switch _go
                        }
            return $ Right True
    else return $ Left (show coord <> " has already been selected\n")

computer :: Board -> Board
computer = undefined

help :: String
help = join [ "TIC TAC TOE HELP MENU\n"
            , "\nCommand  |  Description"
            , "\n---------|------------------------------------------"
            , "\nhelp     |  This Menu (Case insensitive)"
            , "\nexit     |  Quits the game (Case insensitive)"
            , "\n1-9      |  Selects the coordinate labelled the same\n"]
