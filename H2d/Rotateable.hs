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

{-# LANGUAGE FlexibleInstances #-}

module H2d.Rotateable where

import H2d.Vec2D
import H2d.Path2D

class Rotateable a where
    rotate :: a -> Double -> Vec2D -> a

instance Rotateable Path2D where
    rotate path rad center = map(\x -> rotate x rad center) path

instance Rotateable Vec2D where
    rotate (Vec2D x y) rad (Vec2D centerX centerY) = Vec2D newX newY
        where
            newX = centerX + cos rad * (x - centerX) - sin rad * (y - centerY)
            newY = centerY + sin rad * (x - centerX) + cos rad * (y - centerY)
