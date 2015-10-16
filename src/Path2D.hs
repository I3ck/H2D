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

import Control.Parallel.Strategies (runEval, rpar)

import Types2D
import Vec2D
import Debug.Trace

import Data.List
import Data.Ord (comparing)

debug = flip trace

--------------------------------------------------------------------------------

boundingBox :: Path2D -> Path2D
boundingBox    path = [vecMinMin, vecMinMax, vecMaxMin, vecMaxMax]
    where
        vecMinMin = Vec2D (minX path) (minY path)
        vecMinMax = Vec2D (minX path) (maxY path)
        vecMaxMin = Vec2D (maxX path) (minY path)
        vecMaxMax = Vec2D (maxX path) (maxY path)
boundingBox _ = [center, center, center, center]
    where
        center = Vec2D 0.0 0.0

minX :: Path2D -> Double
minX    [(Vec2D x y)] = x
minX    ((Vec2D x y):xs) = min x $ minX xs
minX    _ = 0.0

minY :: Path2D -> Double
minY    [(Vec2D x y)] = y
minY    ((Vec2D x y):xs) = min y $ minY xs
minY    _ = 0.0

maxX :: Path2D -> Double
maxX    [(Vec2D x y)] = x
maxX    ((Vec2D x y):xs) = max x $ maxX xs
maxX    _ = 0.0

maxY :: Path2D -> Double
maxY    [(Vec2D x y)] = y
maxY    ((Vec2D x y):xs) = max y $ maxY xs
maxY    _ = 0.0

--------------------------------------------------------------------------------

sortByX :: Path2D -> Path2D
sortByX    path = sortBy compX path
    where
        compX :: Vec2D -> Vec2D -> Ordering
        compX (Vec2D x1 y1) (Vec2D x2 y2)
            | x1 > x2 = GT
            | x1 < x2 = LT
            | x1 == x2 = compare y1 y2
sortByX _ = []

sortByY :: Path2D -> Path2D
sortByY    path = sortBy compY path
    where
        compY :: Vec2D -> Vec2D -> Ordering
        compY (Vec2D x1 y1) (Vec2D x2 y2)
            | y1 > y2 = GT
            | y1 < y2 = LT
            | y1 == y2 = compare x1 x2
sortByY _ = []

--------------------------------------------------------------------------------

pathLength :: Path2D -> Double
pathLength    (x:y:xs) = distance x y + pathLength xs --TODO possible bug, since it should recurse on (y:xs)
pathLength    _ = 0

pathSize :: Path2D -> Double
pathSize    (x:xs) = 1 + pathSize xs
pathSize    _ = 0

averageDistance :: Path2D -> Double
averageDistance    path = (pathLength path) / (pathSize path - 1)
averageDistance    _ = 0

pathCenter :: Path2D -> Vec2D
pathCenter    path = Vec2D ((sumX path) / vecs) ((sumY path) / vecs)
    where
        vecs = pathSize path

        sumX :: Path2D -> Double
        sumX    ((Vec2D x y):xs) = x + sumX xs
        sumX    _ = 0

        sumY :: Path2D -> Double
        sumY    ((Vec2D x y):xs) = y + sumY xs
        sumY    _ = 0
pathCenter _ = (Vec2D 0.0 0.0)

--------------------------------------------------------------------------------

removeAbove :: Path2D -> Vec2D -> Path2D
removeAbove    path (Vec2D xother yother) = filter belowOrEqual path
    where
        belowOrEqual :: Vec2D -> Bool
        belowOrEqual    (Vec2D xthis ythis) = ythis <= yother
removeAbove _ _ = []

removeBelow :: Path2D -> Vec2D -> Path2D
removeBelow    path (Vec2D xother yother) = filter aboveOrEqual path
    where
        aboveOrEqual :: Vec2D -> Bool
        aboveOrEqual    (Vec2D xthis ythis) = ythis >= yother
removeBelow _ _ = []

removeLeftOf :: Path2D -> Vec2D -> Path2D
removeLeftOf    path (Vec2D xother yother) = filter rightOfOrEqual path
    where
        rightOfOrEqual :: Vec2D -> Bool
        rightOfOrEqual    (Vec2D xthis ythis) = xthis >= xother
removeLeftOf _ _ = []

removeRightOf :: Path2D -> Vec2D -> Path2D
removeRightOf    path (Vec2D xother yother) = filter leftOfOrEqual path
    where
        leftOfOrEqual :: Vec2D -> Bool
        leftOfOrEqual    (Vec2D xthis ythis) = xthis <= xother
removeRightOf _ _ = []

removeCloserTo :: Path2D -> Vec2D -> Double -> Path2D
removeCloserTo    path other minDistance = filter furtherApart path
    where
        furtherApart :: Vec2D -> Bool
        furtherApart    this = distance this other > minDistance
removeCloserTo _ _ _ = []

removeMoreDistantTo :: Path2D -> Vec2D -> Double -> Path2D
removeMoreDistantTo    path      other    maxDistance = filter closerTo path
    where
        closerTo :: Vec2D -> Bool
        closerTo    this = distance this other < maxDistance
removeMoreDistantTo _ _ _ = []

--------------------------------------------------------------------------------

insertAfter :: Int -> Vec2D -> Path2D -> Path2D
insertAfter    pos    vec      path = before ++ (vec:after)
                  where (before,after) = splitAt (pos+1) path

--------------------------------------------------------------------------------

intersectionsPP :: Path2D -> Path2D -> Path2D
intersectionsPP    (p1:p2:ps) other = runEval ( rpar (intersectionsLP (Line2D p1 p2) other)) ++ runEval (rpar (intersectionsPP (p2:ps) other) )
intersectionsPP _ _ = []

--------------------------------------------------------------------------------

intersectionsLP :: Line2D -> Path2D -> Path2D
intersectionsLP    line1     (p3:p4:ps) =  intersectionsLL line1 (Line2D p3 p4) ++ intersectionsLP line1 (p4:ps)
intersectionsLP _ _ = []

--------------------------------------------------------------------------------

intersectionsLL :: Line2D -> Line2D -> Path2D
intersectionsLL    (Line2D p1 p2) (Line2D q1 q2)
    | denominator == 0 = []
    | valid = [(Vec2D intersectX intersectY)]
    | otherwise = []
    where
        valid = (inBetween x1 x2 intersectX) && (inBetween x3 x4 intersectX)
        intersectX = (  (x1*y2 - y1*x2)*(x3 - x4) - (x1 - x2)*(x3*y4 - y3*x4)  ) / denominator
        intersectY = (  (x1*y2 - y1*x2)*(y3 - y4) - (y1 - y2)*(x3*y4 - y3*x4)  ) / denominator

        denominator = (x1 - x2)*(y3 - y4) - (y1 - y2)*(x3 - x4)

        x1 = x p1
        y1 = y p1
        x2 = x p2
        y2 = y p2
        x3 = x q1
        y3 = y q1
        x4 = x q2
        y4 = y q2

        inBetween :: Double -> Double -> Double -> Bool
        inBetween    border1   border2   value = (value >= border1 && value <= border2) || (value >= border2 && value <= border1)
intersectionsLL _ _ = []

--------------------------------------------------------------------------------

createInvolutCircle :: Int -> Double -> Double -> Double -> Vec2D -> Path2D
createInvolutCircle    nPoints diameter radStart  radEnd    (Vec2D centerX centerY) = map involut [0..(nPoints-1)]
    where
        involut :: Int -> Vec2D
        involut i = (Vec2D x y)
            where
                x = centerX + diameter/2.0 * (cos current + current * sin current)
                y = centerY + diameter/2.0 * (sin current - current * cos current)
                current = fromIntegral i * abs (radEnd - radStart) / (fromIntegral nPoints - 1.0)
createInvolutCircle _ _ _ _ _ = []

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
createArc _ _ _ _ _ = []

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
createEllipse _ _ _ _ _ = []

--------------------------------------------------------------------------------

createRectangle :: Double -> Double -> Vec2D -> Path2D
createRectangle    width     height    (Vec2D centerX centerY) = [p1, p2, p3, p4]
    where
        p1 = Vec2D (centerX - width/2.0) (centerY - height/2.0)
        p2 = Vec2D (centerX + width/2.0) (centerY - height/2.0)
        p3 = Vec2D (centerX + width/2.0) (centerY + height/2.0)
        p4 = Vec2D (centerX - width/2.0) (centerY + height/2.0)
createRectangle _ _ _ = []

--------------------------------------------------------------------------------

createLine :: Vec2D -> Vec2D -> Int -> Path2D
createLine    (Vec2D x1 y1) (Vec2D x2 y2) nPoints = map lin [0..(nPoints-1)]
    where
        deltaX = (x2 - x1) / (fromIntegral nPoints - 1.0)
        deltaY = (y2 - y1) / (fromIntegral nPoints - 1.0)
        lin :: Int -> Vec2D
        lin i = (Vec2D x y)
            where
                x = x1 + (fromIntegral i * deltaX)
                y = y1 + (fromIntegral i * deltaY)

--------------------------------------------------------------------------------

interpolationBezier :: Path2D -> Int -> Path2D
interpolationBezier    path      nPoints = map bez [1..(nPoints-1)]
    where
        bez :: Int -> Vec2D
        bez i = bezier path (fromIntegral i * 1.0 / fromIntegral nPoints)

--------------------------------------------------------------------------------

bezier :: Path2D -> Double -> Vec2D
bezier    [x]       _       = x
bezier    path      t       = ( Vec2D{x = 1.0 - t, y = 1.0 - t} * (bezier (init path) t)  ) + Vec2D{x = t, y = t} * (bezier (tail path) t)

--------------------------------------------------------------------------------

-- monotone chain algorithm
-- https://en.wikibooks.org/wiki/Algorithm_Implementation/Geometry/Convex_hull/Monotone_chain#Haskell
convexHull :: Path2D -> Path2D
convexHull    points = lower ++ upper
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
convexHull _ = []

-- TODO good results but way too slow
concaveHull :: Path2D -> Double -> Int -> Path2D
concaveHull    points    minDist   maxIter = buildHull 0 inital --TODO maxIter / iter might be shifted by 1
    where
        inital = convexHull points
        buildHull :: Int -> Path2D -> Path2D
        buildHull iteration hull
            | iteration >= maxIter = hull
            | longestLength < minDist = hull
            | otherwise = buildHull (iteration + 1) $ insertAfter idStartLongest pointWithSmallestAngle hull -- TODO might be ID + 1
                where
                    pointWithSmallestAngle = pCreatingFlatestTriangle points hull pStart pEnd
                    longestLength = distance pStart pEnd
                    pEnd = hull !! idEndLongest
                    pStart = hull !! idStartLongest
                    idStartLongest = startIdOfLongestEdge hull
                    idEndLongest | idStartLongest < (length hull - 1) = idStartLongest + 1
                                 | otherwise = 0

        pCreatingFlatestTriangle :: Path2D -> Path2D -> Vec2D -> Vec2D -> Vec2D -- TODO converting too slowly (maybe angle approach is required after all / or search for steepest, non intersecting triangle)
        pCreatingFlatestTriangle    path      hull      pStart   pEnd = path !! index
            where
                index = fst $ maximumBy (comparing snd) (zip [0..] flats)
                flats = map flatnessOfNewTriangle path

                flatnessOfNewTriangle :: Vec2D -> Double
                flatnessOfNewTriangle p
                    | p `elem` hull = 0
                    | pStart == pEnd = 0
                    | otherwise =  (distance pStart pEnd) / (  (distance pStart p) + (distance p pEnd  )  )


        startIdOfLongestEdge :: Path2D -> Int
        startIdOfLongestEdge    []   = 0
        startIdOfLongestEdge    [x]  = 0
        startIdOfLongestEdge    path = fst $ maximumBy (comparing snd) (zip [0..] ( (distances path) ++ [distance (last path) (head path)] ))

        distances :: Path2D -> [Double]
        distances    []    =  []
        distances    [x]   =  []
        distances (x:y:xs) = ((distance x y) : (distances (y:xs)))
