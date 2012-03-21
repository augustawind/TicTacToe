module TicTacToe
( Game(..)
, Point(..)
, Mark(..)
, EndStatus(..)
, PlayerName
, Direction(..)
-- * Core functions
, newGame
, makePlay
, ticTacToe
-- * Supporting functions
, threeInARow
, inBounds
, isValidPlay
-- * Data.Map function aliases
, markAt
, played
, notPlayed
) where

import Data.Map (Map, empty, member, notMember, insert, fromList, size)
import qualified Data.Map as Map (lookup)


-- |A game is a mapping of points to marks and an end status.
data Game = Game { getPlays :: (Map Point Mark)
                 , getStatus :: EndStatus }
                 deriving (Show)

-- |A point is two coordinates.
data Point = Point Int Int
             deriving (Show, Eq, Ord)

-- |A mark is either an X or O.
data Mark = MarkX | MarkO
            deriving (Show, Eq, Ord)

-- |An end status describes whether a game is over, and how.
data EndStatus = Undecided | Winner Mark
                  deriving (Show, Eq) 

type PlayerName = String

-- |A direction is a cardinal or ordinal point.
data Direction = N | NE | E | SE | S | SW | W | NW
                 deriving (Show, Eq, Enum)


newGame :: Game
newGame = Game empty Undecided


makePlay :: Mark -> Point -> Game -> Game
makePlay mark point g@(Game plays end) = if inBounds point
                                         then Game (insert point mark plays) end
                                         else g


inBounds :: Point -> Bool
inBounds (Point x y) = x >= 0 && x < 3
                    && y >= 0 && y < 3


isValidPlay :: Point -> Game -> Bool
isValidPlay point game = inBounds point && notPlayed point game


ticTacToe :: Mark -> Point -> Game -> Bool
ticTacToe mark p game = or [threeInARow mark p dir game | dir <- [N .. NW]]


threeInARow :: Mark -> Point -> Direction -> Game -> Bool
threeInARow mark (Point x y) dir game = if all inBounds points
                                                  then all (== Just mark) $ marks
                                                  else False
    where marks  = map (`markAt` game) points 
          points = let x1 = if centerX then x-dx else x+dx
                       y1 = if centerY then y-dy else y+dy
                       x2 = if centerX then x+dx else x1+dx
                       y2 = if centerY then y+dy else y1+dy
                       centerX  = x == 1
                       centerY  = y == 1
                       (dx, dy) = dirToXY dir
                   in [Point x y, Point x1 y1, Point x2 y2]


dirToXY :: Direction -> (Int, Int)
dirToXY N  = (0, 1)
dirToXY NE = (1, 1)
dirToXY E  = (1, 0)
dirToXY SE = (1, -1)
dirToXY S  = (0, -1)
dirToXY SW = (-1, -1)
dirToXY W  = (-1, 0)
dirToXY NW = (-1, 1)


markAt :: Point -> Game -> Maybe Mark
markAt point (Game plays _) = Map.lookup point plays


played :: Point -> Game -> Bool
point `played` (Game plays _) = point `member` plays


notPlayed :: Point -> Game -> Bool
point `notPlayed` (Game plays _) = point `notMember` plays
