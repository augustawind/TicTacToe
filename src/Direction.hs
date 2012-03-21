-- |Cardinal and ordinal directions.
module Direction  where


-- |A direction is a cardinal or ordinal point.
data Direction = N | NE | E | SE | S | SW | W | NW
                 deriving (Show, Eq, Enum)

-- |Convert a direction to a directional pair (dx, dy).
direct :: Direction -> (Int, Int)
direct N  = (0, 1)
direct NE = (1, 1)
direct E  = (1, 0)
direct SE = (1, -1)
direct S  = (0, -1)
direct SW = (-1, -1)
direct W  = (-1, 0)
direct NW = (-1, 1)
