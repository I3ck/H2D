{-# LANGUAGE FlexibleInstances #-}
module Parseable where

import Vec2D
import Path2D

--TODO read can be used?
class Parseable a where
    parse :: String -> a
    write :: a -> String

instance Parseable Path2D where
    parse content = map parse lins
        where
            lins = lines content

    write []     = ""
    write (x:xs) = write x ++ "\n" ++ write xs

instance Parseable Vec2D where
    parse line = Vec2D{ x = d1, y = d2 }
        where
            d1 = head dbls
            d2 = head $ tail dbls -- TODO more efficent way?
            dbls = map read  (words line)

    write (Vec2D x y) = show x ++ " " ++ show y
