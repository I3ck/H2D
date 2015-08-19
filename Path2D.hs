{-# LANGUAGE FlexibleInstances #-}

module Path2D where

import Vec2D

type Path2D = [Vec2D]

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
