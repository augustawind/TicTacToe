-- |Game logic for a Tic Tac Toe game.
module TicTacToe
( Game(..)
, Cell(..)
, Mark(..)
, EndStatus(..)
-- * Core functions
, newGame
, makeMove
, ticTacToe
, stalemate
-- * Supporting functions
, threeInARow
, inBounds
, isValidMove
-- ** Data.Map aliases
, markAt
, occupied
, unoccupied
-- * Utility
, updateEndStatus
) where

import Data.Map (Map, empty, member, notMember, insert, fromList, size)
import qualified Data.Map as Map (lookup)

import Direction


-- |A game is a mapping of cells to marks and an end status.
data Game = Game { getMoves :: (Map Cell Mark)
                 , getStatus :: EndStatus }
                 deriving (Show)

-- |A cell is represented by two coordinates.
data Cell = Cell Int Int
            deriving (Show, Eq, Ord)

-- |A mark is either an X or O.
data Mark = MarkX | MarkO
            deriving (Eq, Ord)

instance Show Mark where
    show MarkX = "X"
    show MarkO = "O"

-- |An end status describes whether a game is over, and how.
data EndStatus = Undecided | Stalemate | Winner Mark
                  deriving (Show, Eq) 


-- |Return an empty, undecided game.
newGame :: Game
newGame = Game empty Undecided


-- |Place a mark on a game if the given cell is unoccupied.
makeMove :: Mark -> Cell -> Game -> Game
makeMove mark cell gm@(Game moves end) = if inBounds cell
                                         then Game (insert cell mark moves) end
                                         else error "Invalid cell coordinates"


-- |Are some cell coordinates within the bounds of a 3x3 Tic Tac Toe board?
inBounds :: Cell -> Bool
inBounds (Cell x y) = x >= 0 && x < 3
                    && y >= 0 && y < 3


-- |Is a particular cell a valid place to move in a particular game?
isValidMove :: Cell -> Game -> Bool
isValidMove cell gm = inBounds cell && cell `unoccupied` gm


-- |Does a player win the game at a particular cell?
ticTacToe :: Mark -> Cell -> Game -> Bool
ticTacToe mark cell gm = or [threeInARow mark cell dir gm | dir <- [N .. NW]]


-- |Is a game at a stalemate?
stalemate :: Game -> Bool
stalemate (Game moves _) = size moves == 9


-- |Are there three of a particular mark in a row
-- from a particular cell and direction in a game?
threeInARow :: Mark -> Cell -> Direction -> Game -> Bool
threeInARow mark (Cell x y) dir gm = length marks == 3
                                  && all (== Just mark) marks
    where marks = map (`markAt` gm) cells
          cells = let xs = if (x == 1) then [x-dx, x, x+dx]
                                       else [x, x+dx, x+dx+dx]
                      ys = if (y == 1) then [y-dy, y, y+dy]
                                       else [y, y+dy, y+dy+dy]
                      (dx, dy)   = direct dir
                  in filter inBounds $ zipWith Cell xs ys


-- |Lookup a cell in a game, maybe returning a mark at that cell.
markAt :: Cell -> Game -> Maybe Mark
cell `markAt` (Game moves _) = Map.lookup cell moves


-- |Is a particular cell in a game occupied?
occupied :: Cell -> Game -> Bool
cell `occupied` (Game moves _) = cell `member` moves


-- |Opposite of 'occupied'.
unoccupied :: Cell -> Game -> Bool
cell `unoccupied` (Game moves _) = cell `notMember` moves


-- |Update a game's end status to reflect changes made by X or O.
updateEndStatus :: Mark -> Cell -> Game -> Game
updateEndStatus mark cell gm@(Game moves _) =
    if ticTacToe mark cell gm then
        Game moves (Winner mark)
    else
        if stalemate gm then
            Game moves Stalemate
        else 
            gm
