module Line2D where

import Vec2D
data Line2D = Line2D {
    p1 :: Vec2D,
    p2 :: Vec2D
} deriving (Show, Read)
