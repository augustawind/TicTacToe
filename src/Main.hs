module Main where

import Control.Monad (sequence_)
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import System.IO (hFlush, stdout)
import TicTacToe


main :: IO ()
main = do
    putStrLn "<< TIC TAC TOE >>"
    lineBreak
    nameX <- promptForName "Player X"
    nameO <- promptForName "Player O"
    lineBreak
    printInstructions
    lineBreak
    playRound nameX nameO newGame


-- |Print a newline.
lineBreak :: IO ()
lineBreak = putChar '\n'


-- |Print the game instructions.
printInstructions :: IO ()
printInstructions = do
    putStrLn "Take turns making moves until someone gets a 3-in-a-row."
    putStrLn "Enter each move as two digits separated by a space."
    prompt "Possible coordinates are 0, 1 and 2."
    return ()


type PlayerName = String

-- |Execute a round of the game.
playRound :: PlayerName -> PlayerName -> Game -> IO ()
playRound nameX nameO game = do
    g'@(Game _ statusX) <- takeTurn MarkX nameX game
    case statusX of
        Winner MarkX -> gameOver nameX nameO g'
        Undecided    -> do
            g''@(Game _ statusO) <- takeTurn MarkO nameO g'
            case statusO of
                Winner MarkO -> gameOver nameO nameX g''
                Undecided    -> playRound nameX nameO g''


-- |Prompt a player for a move, and update a game accordingly.
takeTurn :: Mark -> PlayerName -> Game -> IO Game
takeTurn mark name g@(Game plays end) = do
    drawGame g
    point <- promptForPlay name
    if isValidPlay point g then do
        describePlay name point
        let g'@(Game plays' _) = makePlay mark point g
        if ticTacToe mark point g' then
            return $ Game plays' (Winner mark)
        else
            return g'
    else do
        putStrLn $ "Invalid move. Try again." 
        takeTurn mark name g


gameOver :: PlayerName -> PlayerName -> Game -> IO ()
gameOver winner loser game = do
    drawGame game
    putStrLn $ winner ++ " wins! Better luck next time, " ++ loser ++ "."
        

-- |Print a string and then get some input.
prompt :: String -> IO String
prompt st = putStr st >> hFlush stdout >> getLine


-- |Prompt for a player's name.
promptForName :: String -> IO String
promptForName playerSt = prompt $ playerSt ++ ", what is your name? "


-- |Prompt for move coordinates until valid input is given.
promptForPlay :: PlayerName -> IO Point
promptForPlay playerName = do
    input <- prompt ('\n':playerName ++ "'s move: ")
    case input of
        (x:' ':y:[]) -> return $ Point (read [x]) (read [y])
        otherwise    -> do
            putStrLn "Invalid input. Try again."
            promptForPlay playerName


describePlay :: PlayerName -> Point -> IO ()
describePlay name (Point x y) =
    putStrLn $ name ++ " plays at (" ++ show x ++ ", " ++ show y ++ ").\n"


drawGame :: Game -> IO ()
drawGame game = do
        sequence_ . intercalate [lineBreak] . reverse $ actions
        putStrLn ""
    where actions        = [[drawCell x y | x <- [0..2]] | y <- [0..2]]
          drawCell x' y' = putStr (getCellStr (Point x' y') game)


getCellStr :: Point -> Game -> String
getCellStr point game = if point `played` game
                        then show . fromJust . markAt point $ game 
                        else "."
