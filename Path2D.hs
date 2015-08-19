{-# LANGUAGE FlexibleInstances #-}

module Path2D where

import Vec2D

type Path2D = [Vec2D]
instance Moveable Path2D where
    move path delta  = map (move delta) path

--------------------------------------------------------------------------------

pathMirrorV :: Path2D -> Double -> Path2D
pathMirrorV path a = map (flip mirrorV a) path

pathMirrorH :: Path2D -> Double -> Path2D
pathMirrorH path a = map (flip mirrorH a) path

pathMirrorP :: Path2D -> Vec2D -> Path2D
pathMirrorP path a = map (flip mirrorP a) path

--------------------------------------------------------------------------------

pathRotate :: Path2D -> Double -> Vec2D -> Path2D
pathRotate path center rad = map(flip rotate center rad) path --Todo might be broken (rotating center around each point)

--------------------------------------------------------------------------------

pathLength :: Path2D -> Double
pathLength [] = 0
pathLength [x] = 0
pathLength (x:y:xs) = distance x y + pathLength xs

pathSize :: Path2D -> Double
pathSize [] = 0
pathSize (x:xs) = 1 + pathSize xs

--------------------------------------------------------------------------------

pathParse :: String -> Path2D
pathParse content = map parse lins
    where
        lins = lines content
