{-# LANGUAGE FlexibleInstances #-}
module Moveable where

import Vec2D
import Path2D

class Moveable a where
    move :: a -> Vec2D -> a

instance Moveable Vec2D where
    move (Vec2D a b) (Vec2D x y) = Vec2D (a+x) (b+y)

instance Moveable Path2D where
    move path delta  = map (move delta) path
