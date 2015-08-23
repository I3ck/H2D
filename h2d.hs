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

module Main where

import System.Environment
import System.IO
import Control.Monad

--------------------------------------------------------------------------------

import H2d.Vec2D
import H2d.Path2D
import H2d.Line2D

import H2d.Mirrorable
import H2d.Moveable
import H2d.Rotateable
import H2d.Parseable

--------------------------------------------------------------------------------

main = do
    putStrLn "hello"
    let p1 = Vec2D {x = 1.0, y=3.0}
    let p2 = Vec2D {x = 2.0, y=7.0}
    let p3 = move p1 p2
    print p3
    let p4 = mirrorV p3 0.0
    print p4
    let p5 = mirrorH p4 0.0
    print p5
    let p6 = mirrorP p5 Vec2D {x = 0.0, y = 0.0}
    print p6
    let p7 = rotate p6 pi Vec2D{x = 0.0, y=0.0}
    print p7
    let a = absolute p7
    print a
    let p = phi p7
    print p
    let b = distance p7 Vec2D{x = 0.0, y=0.0}
    print b
    let s = slope p7 Vec2D{x = 0.0, y = 0.0}
    print s
    let r = radTo p7 Vec2D{x = 0.0, y = 0.0}
    print r
    let p8 = center p7 Vec2D{x = 0.0, y = 0.0}
    print p8
    let b = equal p8 p7
    print b
    let b2 = equal p7 p7
    print b2
    let b3 = similar p8 p7 0.0
    print b3
    let b4 = similar p8 p7 100.0
    print b4
    let b5 = similar p8 p8 0.0
    print b5

    let path1 = [p1, p2, p3, p4, p5, p6, p7, p8]
    print path1

    let path2 = move path1 p1
    print path2

    let path3 = rotate path2 45 p1
    print path3

    let l1 = Line2D { p1 = p1, p2 = p2 }
    print l1

    let list = []
    contents <- readFile "gear.tmp"
    let gear = parse contents :: Path2D
    let movedGear = move gear p1
    let rotatedGear = rotate gear (pi/2.0) p1
    let invol = createInvolutCircle 1000 50 0.0 pi p1
    let arc = createArc 1000 50 0.0 (1.5*pi) p1
    let ellipse = createEllipse 1000 30 40 pi p1
    let rect = createRectangle 30 50 p1


    writeFile "rotatedGear.tmp" $ write rotatedGear
    writeFile "movedGear.tmp" $ write movedGear
    writeFile "copiedGear.tmp" $ write gear
    writeFile "invol.tmp" $ write invol
    writeFile "arc.tmp" $ write arc
    writeFile "ellipse.tmp" $ write ellipse
    writeFile "rectangle.tmp" $ write rect


    -- testing addition
    let pCenter1 = Vec2D {x = 0.0, y = 0.0}
    let pCenter2 = Vec2D {x = 0.0, y = 200.0}
    let doubleRot = tmp (rotate gear (0.02) pCenter2) 100 0.0002 pCenter1 pCenter2

    writeFile "doubleRot.tmp" $ write doubleRot
    writeFile "doubleRotConvbex.tmp" $ write $ convexHull doubleRot
    writeFile "doubleRotBounding.tmp" $ write $ boundingBox doubleRot

tmp :: Path2D -> Int -> Double -> Vec2D -> Vec2D -> Path2D
tmp    path      times  deltarad  center1  center2 = concat $ map rot [0..(times-1)]
    where
        rot :: Int -> Path2D
        rot i = rotate (rotate path (fromIntegral i * (-deltarad)) center1) (fromIntegral i * deltarad) (rotate center2 (fromIntegral i * (-deltarad)) center1)




f :: [String] -> [Double]
f = map read
