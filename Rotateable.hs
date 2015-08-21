{-# LANGUAGE FlexibleInstances #-}
module Rotateable where

import Vec2D
import Path2D

class Rotateable a where
    rotate :: a -> Double -> Vec2D -> a

instance Rotateable Path2D where
    rotate path rad center = map(\x -> rotate x rad center) path

instance Rotateable Vec2D where
    rotate (Vec2D x y) rad (Vec2D centerX centerY) = Vec2D newX newY
        where
            newX = centerX + cos rad * (x - centerX) - sin rad * (y - centerY)
            newY = centerY + sin rad * (x - centerX) + cos rad * (y - centerY)
