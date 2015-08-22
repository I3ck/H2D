module H2d.Line2D where

import H2d.Vec2D

data Line2D = Line2D {
    p1 :: Vec2D,
    p2 :: Vec2D
} deriving (Show, Read)


-- TODO intersection code https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
