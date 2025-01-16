module Main where

import           Control.Monad       (join, when, void)
import           Control.Monad.State (StateT, evalStateT, get, gets, liftIO,
                                      put)
import           Data.Bifunctor      (Bifunctor (bimap))
import           Data.Char           (toLower, toUpper)
import           Data.List           (transpose)
import           Data.List.Split     (chunksOf)
import           Data.Maybe          (mapMaybe)
import           System.IO           (hFlush, stdout)
import           System.Process      (callCommand)
import           System.Random       (randomRIO)

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

data WDC = Win | Draw | CarryOn deriving Eq

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
  show (Turn _token) = show _token
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
    printRow _       = ""
    addLines [f,s,t] = [f, hor, s, hor, t ]
    addLines _       = []
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

newBoard :: Monad m => (String, String) -> Bool -> m GameState 
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
                        Left _msg -> aiLoop _msg
                        Right bl -> when bl $ aiLoop ""
    _ -> do stillPlaying <- playerGo msg
            case stillPlaying of
              Left err -> aiLoop err
              Right bl -> when bl $ aiLoop ""

randomSelect :: [Int] -> Maybe (IO Int)
randomSelect [] = Nothing
randomSelect xs = Just $ (!!) xs <$> randomRIO (0, length xs - 1)

aiAlgo :: StateT GameState IO (Either Msg Bool)
aiAlgo = do
  current <- get
  let (Game player _bot) = game current
      currentPlayer = go current
      playerName = playerToGoName (go current) (game current) 
      ptoken = token player
      btoken = token _bot
      _board = board current
      botWin = randomSelect =<< checkForWinMve btoken _board
      playerWin = randomSelect =<< checkForWinMve ptoken _board
      noWin = randomSelect $ boardCoordsLeft _board
  case botWin of
    Just ioInt -> do pos <- liftIO ioInt
                     void $ updateBoard pos
                     stillGoing <- winDrawCarryOn
                     new <- get
                     updateGame stillGoing currentPlayer
                     liftIO $ botWinner stillGoing playerName new
                     return $ Left ""
    Nothing -> case playerWin of
                Just ioInt -> do 
                  pos <- liftIO ioInt
                  void $ updateBoard pos
                  stillGoing <- winDrawCarryOn
                  new <- get 
                  updateGame stillGoing currentPlayer
                  liftIO $ botWinner stillGoing playerName new
                  return $ Left ""
                Nothing -> case noWin of 
                  Nothing -> do 
                    stillGoing <- winDrawCarryOn
                    new <- get 
                    updateGame stillGoing currentPlayer
                    liftIO $ botWinner stillGoing playerName new
                    return $ Left ""
                  Just ioInt  -> do 
                    pos <- liftIO ioInt
                    void $ updateBoard pos
                    stillGoing <- winDrawCarryOn
                    updateGame stillGoing currentPlayer
                    return $ Left "" 

botWinner :: WDC -> String -> GameState -> IO () 
botWinner stillGoing playerName current = do 
  let _game = game current 
      _board = board current 
  callCommand "clear"
  print _game 
  printGame _board 
  printWinner stillGoing playerName

boardCoordsLeft :: Board -> [Int]
boardCoordsLeft = fmap fst . filter (isBlank . snd) . concat . zipWith zip positions
  where
    positions     = [[1,2,3],[4,5,6],[7,8,9]] :: [[Int]]
    isBlank Blank = True
    isBlank _     = False

playerGo :: String -> StateT GameState IO (Either Msg Bool)
playerGo msg = do
  current <- get
  let currentPlayer = go current
      playerName = playerToGoName (go current) (game current) 
  liftIO $ do
    callCommand "clear"
    print $ game current
    printGame $ board current
    putStrLn msg
    putStr $ playerName <> " Enter command: "
    hFlush stdout
  input <- liftIO getLine
  stillPlaying <- handleInput (toLower <$> input) -- handleInput needs to be before checkGame
  stillGoing <- winDrawCarryOn
  liftIO $ printWinner stillGoing playerName
  updateGame stillGoing currentPlayer
  return stillPlaying

printWinner :: WDC -> String -> IO ()
printWinner Win _name = do 
  putStrLn $ toUpper <$> _name <> " WON!!!!!\nPress Enter to continue"
  hFlush stdout
  _ <- getLine
  return ()
printWinner Draw _name = do 
  putStrLn "It was a Draw!"
  hFlush stdout
  _ <- getLine
  return ()
printWinner _ _ = return ()

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

updateGame :: WDC -> Token -> StateT GameState IO ()
updateGame CarryOn _ = return ()
updateGame wd player = do
  current <- get
  let playersTuple (Game p1 p2) = (p1, p2)
      inc    = if wd == Win then 1 else 0
      _game  = game current
      names  = join bimap name $ playersTuple _game
      scores = addToPlayersScore inc player $ playersTuple _game
      tokens = join bimap switch $ join bimap token $ playersTuple _game
  put $ updateGameState blankBoard names scores tokens (switch $ go current)

addToPlayersScore :: Int -> Token -> (Player, Player) -> (Int, Int)
addToPlayersScore inc t (p1, p2) | token p1 == t = ((+inc) $ score p1, score p2)
                                 | otherwise     = (score p1, (+inc) $ score p2)

winDrawCarryOn :: StateT GameState IO WDC 
winDrawCarryOn = wdc <$> checkGame <*> allMoves

wdc :: Bool -> Bool -> WDC
wdc wins draws | wins      = Win
               | draws     = Draw
               | otherwise = CarryOn

allMoves :: StateT GameState IO Bool 
allMoves = gets $ notElem Blank . concat . board

checkGame :: StateT GameState IO Bool
checkGame = gets $ checkRows . allCombinations . board

checkRows :: [[Coord]] -> Bool
checkRows   = any ( \x -> all (== Turn O) x || all (== Turn X) x )

checkForWinMve :: Token -> [[Coord]] -> Maybe [Int]
checkForWinMve _token _board = case matched2 of
  []   -> Nothing
  ints -> Just ints
  where
    positions = [[1,2,3],[4,5,6],[7,8,9]]
    coordPos  = zipWith zip positions _board
    matched2  = mapMaybe (match2 _token) (allCombinations coordPos)

match2 :: Token -> [(Int, Coord)] -> Maybe Int
match2 _token [a, b, c] | Turn _token == snd a && Turn _token == snd b && snd c == Blank = Just $ fst c
                        | Turn _token == snd a && Turn _token == snd c && snd b == Blank = Just $ fst b
                        | Turn _token == snd b && Turn _token == snd c && snd a == Blank = Just $ fst a
                        | otherwise = Nothing
match2 _ _ = Nothing

allCombinations :: [[a]] -> [[a]]
allCombinations b = b <> transpose b <> diagonals b

diagonals :: [[a]] -> [[a]]
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
