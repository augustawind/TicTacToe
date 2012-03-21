-- |Cardinal and ordinal directions.
module Direction  where


-- |A direction is a cardinal or ordinal point.
data Direction = N | NE | E | SE | S | SW | W | NW
                 deriving (Show, Eq, Enum)


-- |Convert a direction to a directional pair (dx, dy).
direction :: Direction -> (Int, Int)
direction N  = (0, 1)
direction NE = (1, 1)
direction E  = (1, 0)
direction SE = (1, -1)
direction S  = (0, -1)
direction SW = (-1, -1)
direction W  = (-1, 0)
direction NW = (-1, 1)
