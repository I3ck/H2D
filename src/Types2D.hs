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

module Types2D where

data Vec2D = Vec2D {
    x :: Double,
    y :: Double
} deriving (Show, Read)

instance Eq Vec2D where
    (==) (Vec2D x1 y1) (Vec2D x2 y2) = x1 == x2 && y1 == y2
    (/=) (Vec2D x1 y1) (Vec2D x2 y2) = x1 /= x2 || y1 /= y2

instance Ord Vec2D where
    (<) (Vec2D x1 y1) (Vec2D x2 y2) = x1 < x2 || (x1 == x2 && y1 < y2)
    (<=) vec1 vec2 = vec1 < vec2 || vec1 == vec2
    (>) (Vec2D x1 y1) (Vec2D x2 y2) = x1 > x2 || (x1 == x2 && y1 > y2)
    (>=) vec1 vec2 = vec1 > vec2 || vec1 == vec2

instance Num Vec2D where
   (Vec2D a b) + (Vec2D c d) = Vec2D (a+c) (b+d)
   (Vec2D a b) * (Vec2D c d) = Vec2D (a*c) (b*d)
   (Vec2D a b) - (Vec2D c d) = Vec2D (a-c) (b-d)
   abs    (Vec2D a b) = Vec2D (abs a) (abs b)
   signum (Vec2D a b) = Vec2D (signum a) (signum b)
   fromInteger i = Vec2D (fromInteger i) (fromInteger i)

type IdVec2D = (Int, Vec2D)

--------------------------------------------------------------------------------

data Line2D = Line2D {
    p1 :: Vec2D,
    p2 :: Vec2D
} deriving (Show, Read)

--------------------------------------------------------------------------------

type Path2D = [Vec2D]
type IdPath2D = [IdVec2D]

--------------------------------------------------------------------------------

data KdTree2D v  = Node { left   :: KdTree2D v
                        , val    :: v
                        , right  :: KdTree2D v
                        , axis   :: Int
                        }
                 | Kempty
    deriving (Eq, Ord, Show)

instance Foldable KdTree2D where
    foldr f init Kempty = init
    foldr f init (Node left val right axis) = foldr f z3 left
        where
            z3 = f val z2
            z2 = foldr f init right

instance Functor KdTree2D where
    fmap _ Kempty = Kempty
    fmap f (Node left val right axis) = Node (fmap f left) (f val) (fmap f right) axis

--------------------------------------------------------------------------------
