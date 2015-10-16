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

module HighOrderFunctions2D where

import Data.List
import Data.Ord (comparing)

import Types2D
import Vec2D
import Path2D
import KdTree2D

concaveHullKNearest :: Path2D -> Int -> Int -> Path2D
concaveHullKNearest    points    kNearest maxIter = (buildHull [startPoint] 0) ++ [startPoint]
    where
        startPoint = head pSorted
        pSorted = sortByX points

        buildHull :: Path2D -> Int -> Path2D
        buildHull [] _ = []
        buildHull hull iter
            | kNearest > length pSorted = hull
            | iter >= maxIter = hull
            | (pSorted !! next) `elem` hull = hull
            | otherwise = buildHull (hull ++ [(pSorted !! next)]) (iter+1)
                where
                    p = last hull

                    idCandidates = take kNearest $ map fst $  sortBy (comparing  snd) (zip [0..] distancesToP)
                        where distancesToP = map distanceToP pSorted
                    next = chooseNext idCandidates

                    distanceToP :: Vec2D -> Double
                    distanceToP   x
                        | p == x = 1e300
                        | x /= startPoint && x `elem` hull = 1e300 -- TODO choose biggest possible number here
                        | otherwise = sqrDistance p x

                    chooseNext :: [Int] -> Int
                    chooseNext    idCandidates = fst $ maximumBy compCcw (zip idCandidates pCandidates)
                        where
                            compCcw :: IdVec2D -> IdVec2D -> Ordering
                            compCcw (_, v1) (_, v2)
                                | v1 == startPoint = GT
                                | v2 == startPoint = LT

                                | v1 `elem` hull && not(v2 `elem` hull) = LT
                                | not(v1 `elem` hull) && v2 `elem` hull = GT
                                | v1 `elem` hull && v2 `elem` hull = EQ

                                | turn p v1 v2 > 0 = GT
                                | turn p v1 v2 < 0 = LT

                                | sqrDistance p v1 < sqrDistance p v2 = LT
                                | sqrDistance p v1 > sqrDistance p v2 = GT

                                | otherwise = EQ
                                where
                                    turn :: Vec2D -> Vec2D -> Vec2D -> Int
                                    turn p q r
                                        | ccw ( dir p q) (dir q r) = 1
                                        | cw ( dir p q) (dir q r)  = -1
                                        | otherwise = 0
                            pCandidates = map (pSorted !!) idCandidates
