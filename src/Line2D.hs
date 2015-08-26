{-
Copyright 2015 Martin Buck
This file is part of H2D.
H2D is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.
H2D is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.
You should have received a copy of the GNU Lesser General Public License
along with H2D.  If not, see <http://www.gnu.org/licenses/>.
-}

module Line2D where

import Vec2D

data Line2D = Line2D {
    p1 :: Vec2D,
    p2 :: Vec2D
} deriving (Show, Read)


-- TODO intersection code https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
