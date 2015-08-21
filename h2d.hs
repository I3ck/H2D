-- Todo add tests for everything
-- Todo path is missing many methods
-- Todo path made of line segments
-- Todo sweep intersections
-- Todo define path as line segments or
-- Todo use classes like Moveable which define a move method, then derive from them
-- Todo instanciate show and read instead of parse and write
-- Todo fromIntegral realToFrac
-- Todo drop angle as parameter from ellipse?
-- Todo add interpolations
-- Todo add all path algorithms
-- Todo add sweep intersection algorithms
-- Todo make this a proper package useable by others
-- Todo add a license
-- Todo release on github?

--------------------------------------------------------------------------------

import System.Environment
import System.IO
import Control.Monad


--------------------------------------------------------------------------------

import Vec2D
import Path2D
import Line2D

import Mirrorable
import Moveable
import Rotateable
import Parseable

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






f :: [String] -> [Double]
f = map read
