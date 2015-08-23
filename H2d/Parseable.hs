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
module H2d.Parseable where

import H2d.Vec2D
import H2d.Path2D

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
