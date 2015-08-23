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
module H2D.Mirrorable where

import H2D.Vec2D
import H2D.Path2D

class Mirrorable a where
    mirrorV :: a -> Double -> a
    mirrorH :: a -> Double -> a
    mirrorP :: a -> Vec2D -> a

instance Mirrorable Vec2D where
    mirrorV (Vec2D x y) a           = Vec2D (2 * a - x) y
    mirrorH (Vec2D x y) a           = Vec2D x (2 * a - y)
    mirrorP (Vec2D x y) (Vec2D a b) = Vec2D (2 * a - x) (2 * b - y)

instance Mirrorable Path2D where
    mirrorV path a = map (flip mirrorV a) path
    mirrorH path a = map (flip mirrorH a) path
    mirrorP path a = map (flip mirrorP a) path
