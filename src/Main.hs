-- |A 2-player Tic Tac Toe game.
module Main 
( main
, printInstructions
, playRound
, takeTurn
, drawGame
, describeMove
, gameWon
, gameStalemate
, promptForName
, promptForMove
, showCell
) where

import Control.Monad (sequence_, when, unless)
import Data.Char (isSpace, intToDigit)
import Data.List (intercalate)
import Data.Maybe (fromJust)

import TicTacToe
import Util


-- |Print a title, ask for the players' names, print some instructions
-- and start the game.
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


-- |Print the game instructions.
printInstructions :: IO ()
printInstructions = do
    putStrLn "Take turns making moves until someone gets a 3-in-a-row."
    putStrLn "Enter each move as two digits separated by a space."
    prompt "Possible coordinates are 1, 2 and 3."
    return ()


-- |Name to print when referring to a particular player.
type PlayerName = String

-- |Execute a round of the game.
playRound :: PlayerName -> PlayerName -> Game -> IO ()
playRound nameX nameO game = do
    game'@(Game _ statusX) <- takeTurn MarkX nameX game
    case statusX of
        Winner MarkX -> gameWon nameX nameO game'
        Stalemate    -> gameStalemate game'
        Undecided    -> do

            game''@(Game _ statusO) <- takeTurn MarkO nameO game'
            case statusO of
                Winner MarkO -> gameWon nameO nameX game''
                Stalemate    -> gameStalemate game'
                Undecided    -> playRound nameX nameO game''


-- |Prompt X or O for a move, returning an updated game.
takeTurn :: Mark -> PlayerName -> Game -> IO Game
takeTurn mark name game@(Game plays end) = do
    drawGame game
    cellInput@(Cell x y) <- promptForMove name
    let cell = Cell (x-1) (y-1)
    if isValidMove cell game then do
        describeMove name cellInput
        return (updateEndStatus mark cell (makeMove mark cell game))
        else do
            putStrLn "Invalid move. Try again."
            takeTurn mark name game


-- |Draw a game on the screen.
drawGame :: Game -> IO ()
drawGame game = do lineBreak
                   sequence_ . intercalate [lineBreak] . reverse $ actions
                   lineBreak
    where actions = map (map putChar) chars
          chars   = map (map (`showCell` game)) pts
          pts     = [[Cell x y | x <- [0..2]] | y <- [0..2]]


-- |Print some information about a player's move at a cell.
describeMove :: PlayerName -> Cell -> IO ()
describeMove name (Cell x y) =
    putStrLn $ name ++ " moves at (" ++ show x ++ ", " ++ show y ++ ")."


-- |Draw the final game and congratulate the winner.
gameWon :: PlayerName -> PlayerName -> Game -> IO ()
gameWon winner loser gm = do
    drawGame gm
    lineBreak
    putStrLn $ winner ++ " wins! Better luck next time, " ++ loser ++ "."


-- |Draw the final game and empathize over the lack of a firm resolution.
gameStalemate :: Game -> IO ()
gameStalemate gm = do
    drawGame gm
    lineBreak
    putStrLn "It's a stalemate! Bummer, man."


-- |Prompt for a player's name.
promptForName :: String -> IO String
promptForName = prompt . (++ ", what is your name? ")


-- |Prompt for move coordinates until valid input is given.
promptForMove :: PlayerName -> IO Cell
promptForMove name = do
    input <- prompt ('\n':name ++ "'s move: ")
    case input of
        (x:' ':y:[]) -> return $ Cell (read [x]) (read [y])
        otherwise    -> do
            putStrLn "Invalid input. Try again."
            promptForMove name


-- |Return a suitable display char for a given cell in a game.
showCell :: Cell -> Game -> Char
showCell cell game = if cell `occupied` game
                     then head . show . fromJust . markAt cell $ game
                     else '.'
