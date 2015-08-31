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

module Path2D where

import Vec2D
import Line2D

import Data.List

type Path2D = [Vec2D]

--------------------------------------------------------------------------------

boundingBox :: Path2D -> Path2D --TODO rather make this return four points?
boundingBox path = [vecMin, vecMax]
    where
        vecMin = Vec2D (minX path) (minY path)
        vecMax = Vec2D (maxX path) (maxY path)

minX :: Path2D -> Double
minX [(Vec2D x y)] = x
minX ((Vec2D x y):xs) = min x $ minX xs

minY :: Path2D -> Double
minY [(Vec2D x y)] = y
minY ((Vec2D x y):xs) = min y $ minY xs

maxX :: Path2D -> Double
maxX [(Vec2D x y)] = x
maxX ((Vec2D x y):xs) = max x $ maxX xs

maxY :: Path2D -> Double
maxY [(Vec2D x y)] = y
maxY ((Vec2D x y):xs) = max y $ maxY xs

--------------------------------------------------------------------------------

sortByX :: Path2D -> Path2D
sortByX path = sortBy compX path
    where
        compX :: Vec2D -> Vec2D -> Ordering
        compX (Vec2D x1 y1) (Vec2D x2 y2)
            | x1 > x2 = GT
            | x1 < x2 = LT
            | x1 == x2 = compare y1 y2

sortByY :: Path2D -> Path2D
sortByY path = sortBy compY path
    where
        compY :: Vec2D -> Vec2D -> Ordering
        compY (Vec2D x1 y1) (Vec2D x2 y2)
            | y1 > y2 = GT
            | y1 < y2 = LT
            | y1 == y2 = compare x1 x2

--------------------------------------------------------------------------------
pathLength :: Path2D -> Double
pathLength [] = 0
pathLength [x] = 0
pathLength (x:y:xs) = distance x y + pathLength xs

pathSize :: Path2D -> Double
pathSize [] = 0
pathSize (x:xs) = 1 + pathSize xs

averageDistance :: Path2D -> Double
averageDistance [] = 0
averageDistance [x] = 0
averageDistance path = (pathLength path) / (pathSize path - 1)

pathCenter :: Path2D -> Vec2D
pathCenter [] = (Vec2D 0.0 0.0)
pathCenter path = Vec2D ((sumX path) / vecs) ((sumY path) / vecs)
    where
        vecs = pathSize path

        sumX :: Path2D -> Double
        sumX [] = 0
        sumX ((Vec2D x y):xs) = x + sumX xs

        sumY :: Path2D -> Double
        sumY [] = 0
        sumY ((Vec2D x y):xs) = y + sumY xs

--------------------------------------------------------------------------------

removeAbove :: Path2D -> Vec2D -> Path2D
removeAbove [] _ = []
removeAbove path (Vec2D xother yother) = filter belowOrEqual path
    where
        belowOrEqual :: Vec2D -> Bool
        belowOrEqual    (Vec2D xthis ythis) = ythis <= yother

removeBelow :: Path2D -> Vec2D -> Path2D
removeBelow [] _ = []
removeBelow path (Vec2D xother yother) = filter aboveOrEqual path
    where
        aboveOrEqual :: Vec2D -> Bool
        aboveOrEqual    (Vec2D xthis ythis) = ythis >= yother

removeLeftOf :: Path2D -> Vec2D -> Path2D
removeLeftOf [] _ = []
removeLeftOf path (Vec2D xother yother) = filter rightOfOrEqual path
    where
        rightOfOrEqual :: Vec2D -> Bool
        rightOfOrEqual    (Vec2D xthis ythis) = xthis >= xother

removeRightOf :: Path2D -> Vec2D -> Path2D
removeRightOf [] _ = []
removeRightOf path (Vec2D xother yother) = filter leftOfOrEqual path
    where
        leftOfOrEqual :: Vec2D -> Bool
        leftOfOrEqual    (Vec2D xthis ythis) = xthis <= xother

removeCloserTo :: Path2D -> Vec2D -> Double -> Path2D
removeCloserTo [] _ _ = []
removeCloserTo path other minDistance = filter furtherApart path
    where
        furtherApart :: Vec2D -> Bool
        furtherApart    this = distance this other > minDistance

removeMoreDistantTo :: Path2D -> Vec2D -> Double -> Path2D
removeMoreDistantTo [] _ _ = []
removeMoreDistantTo path other maxDistance = filter closerTo path
    where
        closerTo :: Vec2D -> Bool
        closerTo    this = distance this other < maxDistance

--------------------------------------------------------------------------------
-- TODO rename
intersections :: Path2D -> Path2D -> Path2D
intersections (p1:p2:ps) other = intersectionsLP (Line2D p1 p2) other ++ intersections ps other
intersections _ _ = []

--------------------------------------------------------------------------------

-- TODO rename
intersectionsLP :: Line2D -> Path2D -> Path2D
intersectionsLP line1 (p3:p4:ps) =  intersectionsLL line1 (Line2D p3 p4) ++ intersectionsLP line1 ps
intersectionsLP _ _ = []

--------------------------------------------------------------------------------

-- TODO rename
intersectionsLL :: Line2D -> Line2D -> Path2D
intersectionsLL (Line2D p1 p2) (Line2D q1 q2)
    | pVertical && qVertical = []
    | pHorizontal && qHorizontal = []
    | valid = [(Vec2D intersectX intersectY)]
    | otherwise = []
    where
        pVertical = x p1 == x p2
        pHorizontal = y p1 == y p2
        qVertical = x q1 == x q2
        qHorizontal = y q1 == y q2
        valid = inBetween (min (x q1) (x q2)) (max (x q1) (x q2)) intersectX &&
                inBetween (min (x p1) (x p2)) (max (x p1) (x p2)) intersectX
        intersectX = (y q1 - y p1 + slope p1 p2 * x p1 - slope q1 q2 * x q1) /  (slope p1 p2 - slope q1 q2)
        intersectY =  slope p1 p2 * (intersectX - x p1) + y p1

        inBetween :: Double -> Double -> Double -> Bool -- TODO move to general util file
        inBetween border1 border2 value = (value >= border1 && value <= border2) || (value >= border2 && value <= border1)

--------------------------------------------------------------------------------

createInvolutCircle :: Int -> Double -> Double -> Double -> Vec2D -> Path2D
createInvolutCircle    nPoints diameter radStart radEnd    (Vec2D centerX centerY) = map involut [0..(nPoints-1)]
    where
        involut :: Int -> Vec2D
        involut i = (Vec2D x y)
            where
                x = centerX + diameter/2.0 * (cos current + current * sin current)
                y = centerY + diameter/2.0 * (sin current - current * cos current)
                current = fromIntegral i * abs (radEnd - radStart) / (fromIntegral nPoints - 1.0)

--------------------------------------------------------------------------------

createArc :: Int -> Double -> Double -> Double -> Vec2D -> Path2D
createArc    nPoints diameter radStart  radEnd    (Vec2D centerX centerY) = map arc [0..(nPoints-1)]
    where
        arc :: Int -> Vec2D
        arc i = (Vec2D x y)
            where
                x = centerX + diameter/2.0 * cos(radians);
                y = centerY + diameter/2.0 * sin(radians);
                radians = radStart + fromIntegral i * abs (radEnd - radStart) / (fromIntegral nPoints - 1.0)

--------------------------------------------------------------------------------

createEllipse :: Int -> Double -> Double -> Double -> Vec2D -> Path2D
createEllipse    nPoints a        b         rad       (Vec2D centerX centerY) = map ell [0..(nPoints-1)]
    where
        ell :: Int -> Vec2D
        ell i = (Vec2D x y)
            where
                x = centerX + a * cos current * cos rad - b * sin current * sin rad
                y = centerY + a * cos current * sin rad + b * sin current * cos rad
                current = fromIntegral i * 2.0 * pi / (fromIntegral nPoints - 1.0)

--------------------------------------------------------------------------------

createRectangle :: Double -> Double -> Vec2D -> Path2D
createRectangle    width     height    (Vec2D centerX centerY) = [p1, p2, p3, p4]
    where
        p1 = Vec2D (centerX - width/2.0) (centerY - height/2.0)
        p2 = Vec2D (centerX + width/2.0) (centerY - height/2.0)
        p3 = Vec2D (centerX + width/2.0) (centerY + height/2.0)
        p4 = Vec2D (centerX - width/2.0) (centerY + height/2.0)

--------------------------------------------------------------------------------

-- monotone chain algorithm
-- https://en.wikibooks.org/wiki/Algorithm_Implementation/Geometry/Convex_hull/Monotone_chain#Haskell
convexHull :: Path2D -> Path2D
convexHull [] = []
convexHull [p] = [p]
convexHull points = lower ++ upper
  where
    sorted = sort points
    lower = monotoneChain sorted
    upper = monotoneChain (reverse sorted)

    monotoneChain :: Path2D -> Path2D
    monotoneChain = build []
      where
        build :: Path2D -> Path2D -> Path2D
        build acc@(v1:v2:vs) (x:xs) =
          if clockwise v2 v1 x
            then build (v2:vs) (x:xs)
            else build (x:acc) xs
        build acc (x:xs) = build (x:acc) xs
        build acc [] = reverse $ tail acc
