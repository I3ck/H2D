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

module Vec2D where

import Types2D

dir :: Vec2D -> Vec2D -> Vec2D
dir (Vec2D x1 y1) (Vec2D x2 y2) = norm $ Vec2D (x2 - x1) (y2 - y1)

--------------------------------------------------------------------------------

norm :: Vec2D -> Vec2D
norm (Vec2D x y) = Vec2D (x / l) (y / l)
    where l = absolute $ Vec2D  x y

--------------------------------------------------------------------------------

dot :: Vec2D -> Vec2D -> Double
dot    (Vec2D x1 y1) (Vec2D x2 y2) = x1 * x2 + y1 * y2

cross :: Vec2D -> Vec2D -> Double
cross    (Vec2D x1 y1) (Vec2D x2 y2) = x1 * y2 - x2 * y1

--------------------------------------------------------------------------------

cw :: Vec2D -> Vec2D -> Bool
cw v1 v2 = cross v1 v2 < 0

ccw :: Vec2D -> Vec2D -> Bool
ccw v1 v2 = cross v1 v2 > 0

colinear :: Vec2D -> Vec2D -> Bool
colinear v1 v2 = cross v1 v2 == 0

--------------------------------------------------------------------------------

sub :: Vec2D -> Vec2D -> Vec2D
sub    (Vec2D x1 y1) (Vec2D x2 y2) = Vec2D (x1 - x2) (y1 - y2)

--------------------------------------------------------------------------------

clockwise :: Vec2D -> Vec2D -> Vec2D -> Bool
clockwise    o        a        b = (a `sub` o) `cross` (b `sub` o) <= 0

--------------------------------------------------------------------------------

absolute :: Vec2D -> Double
absolute    (Vec2D x y) = sqrt ((x*x) + (y*y))

--------------------------------------------------------------------------------

phi :: Vec2D -> Double
phi    (Vec2D x y) = atan2 y x

--------------------------------------------------------------------------------

distance :: Vec2D -> Vec2D -> Double
distance    v1 v2 = sqrt( sqrDistance v1 v2 )

sqrDistance :: Vec2D -> Vec2D -> Double
sqrDistance    (Vec2D x y) (Vec2D a b) = ((x-a)**2) + ((y-b)**2)

compDistance :: Vec2D -> Vec2D -> Vec2D -> Ordering
compDistance a v1 v2 = sqrDistance a v1 `compare` sqrDistance a v2

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

slope :: Vec2D -> Vec2D -> Double
slope    (Vec2D x y) (Vec2D a b) = (b - y) / (a - x)

--------------------------------------------------------------------------------

radTo :: Vec2D -> Vec2D -> Double
radTo    (Vec2D x y) (Vec2D a b) = atan2 (b - y) (a - x)

--------------------------------------------------------------------------------

center :: Vec2D -> Vec2D -> Vec2D
center    (Vec2D x y) (Vec2D a b) = Vec2D newX newY
    where
        newX = x + (a - x) / 2.0
        newY = y + (b - y) / 2.0

--------------------------------------------------------------------------------

equal :: Vec2D -> Vec2D -> Bool
equal    (Vec2D x y) (Vec2D a b) = res
    where
        res = x == a && y == b

similar :: Vec2D -> Vec2D -> Double -> Bool
similar    v1       v2       delta = res <= delta
    where
        res = distance v1 v2

--------------------------------------------------------------------------------
