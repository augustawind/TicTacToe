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
-- * Supporting functions
, threeInARow
, inBounds
, isValidMove
-- ** Data.Map aliases
, markAt
, occupies
, xOccupies
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
data EndStatus = Undecided | Winner Mark
                  deriving (Show, Eq) 


newGame :: Game
newGame = Game empty Undecided


makeMove :: Mark -> Cell -> Game -> Game
makeMove mark cell g@(Game moves end) = if inBounds cell
                                        then Game (insert cell mark moves) end
                                        else g


inBounds :: Cell -> Bool
inBounds (Cell x y) = x >= 0 && x < 3
                    && y >= 0 && y < 3


isValidMove :: Cell -> Game -> Bool
isValidMove cell game = inBounds cell && xOccupies cell game


ticTacToe :: Mark -> Cell -> Game -> Bool
ticTacToe mark p game = or [threeInARow mark p dir game | dir <- [N .. NW]]


threeInARow :: Mark -> Cell -> Direction -> Game -> Bool
threeInARow mark (Cell x y) dir game = if all inBounds cells
                                        then all (== Just mark) marks
                                        else False
    where marks  = map (`markAt` game) cells 
          cells = let x1 = if centerX then x-dx else x+dx
                      x2 = if centerX then x+dx else x1+dx
                      y1 = if centerY then y-dy else y+dy
                      y2 = if centerY then y+dy else y1+dy
                      centerX  = x == 1
                      centerY  = y == 1
                      (dx, dy) = direction dir
                  in [Cell x y, Cell x1 y1, Cell x2 y2]


markAt :: Cell -> Game -> Maybe Mark
markAt cell (Game moves _) = Map.lookup cell moves


occupies :: Cell -> Game -> Bool
cell `occupies` (Game moves _) = cell `member` moves


xOccupies :: Cell -> Game -> Bool
cell `xOccupies` (Game moves _) = cell `notMember` moves
