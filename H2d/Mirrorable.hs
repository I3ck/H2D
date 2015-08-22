{-# LANGUAGE FlexibleInstances #-}
module H2d.Mirrorable where

import H2d.Vec2D
import H2d.Path2D

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
