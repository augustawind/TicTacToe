module Main where

import Control.Monad (sequence_)
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import System.IO (hFlush, stdout)
import TicTacToe


main :: IO ()
main = do
    putStrLn "<< TIC TAC TOE >>\n"
    nameX <- promptForName "Player X"
    nameO <- promptForName "Player O"
    presentInstructions
    mainLoop nameX nameO newGame


presentInstructions :: IO ()
presentInstructions = do
    putStrLn "\nTake turns making moves until someone gets a 3-in-a-row."
    putStrLn "Enter each move as two digits separated by a space."
    prompt "Possible coordinates are 0, 1 and 2.\n"
    return ()


mainLoop :: PlayerName -> PlayerName -> Game -> IO ()
mainLoop nameX nameO game = do
    drawGame game
    g'@(Game _ statusX) <- takePlayerTurn X nameX game
    case statusX of
        Winner nameX -> gameOver nameX nameO g'
        Undecided    -> do
            drawGame g'
            g''@(Game _ statusO) <- takePlayerTurn O nameO g'
            case statusO of
                Winner nameO -> gameOver nameO nameX g''
                Undecided    -> mainLoop nameX nameO g''


takePlayerTurn :: Mark -> PlayerName -> Game -> IO Game
takePlayerTurn mark name g@(Game plays end) = do
    point <- promptForPlay name
    if isValidPlay point g then do
        describePlay name point
        let g'@(Game plays' _) = makePlay mark point g
        if ticTacToe mark point g' then
            return $ Game plays' (Winner name)
        else
            return g'
    else do
        putStrLn $ "Invalid move." 
        takePlayerTurn mark name g


gameOver :: PlayerName -> PlayerName -> Game -> IO ()
gameOver winner loser game = do
    drawGame game
    putStrLn $ winner ++ " wins! Better luck next time, " ++ loser ++ "."
    return ()
        

prompt :: String -> IO String
prompt st = do
    putStr st
    hFlush stdout
    getLine


promptForName :: String -> IO String
promptForName playerSt = prompt $ playerSt ++ ", what is your name? "


promptForPlay :: PlayerName -> IO Point
promptForPlay playerName = do
    input <- prompt ('\n':playerName ++ "'s move: ")
    case input of
        (x:' ':y:[]) -> parseMove input
        otherwise    -> do
            putStrLn "Invalid input. Try again."
            promptForPlay playerName
    where parseMove coordStr = do
              let xy = break isSpace coordStr
              return $ Point (read (fst xy)) (read (snd xy))


describePlay :: PlayerName -> Point -> IO ()
describePlay name (Point x y) =
    putStrLn $ name ++ " plays at (" ++ show x ++ ", " ++ show y ++ ").\n"


drawGame :: Game -> IO ()
drawGame game = do
        sequence_ $ intercalate [putChar '\n']
                                [[drawCell x y | x <- [0..2]] | y <- [0..2]]
        putStrLn ""
        return ()
    where drawCell x' y' = do putStr (getCellStr (Point x' y') game)


getCellStr :: Point -> Game -> String
getCellStr point game = if point `played` game
                        then show . fromJust . markAt point $ game 
                        else "."
