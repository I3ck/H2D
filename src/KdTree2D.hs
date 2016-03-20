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

-- TODO create a version which stores the ids of points (required for concave)

module KdTree2D where

import Data.List (minimumBy)
import Data.Maybe (fromJust, maybeToList)

import Types2D
import Vec2D
import Path2D

tree2list :: KdTree2D Vec2D -> Path2D
tree2list tree = foldr (:) [] tree

generateTree :: Path2D -> KdTree2D Vec2D
generateTree ps = buildTree ps 0

buildTree :: Path2D -> Int -> KdTree2D Vec2D
buildTree [] _ = Kempty
buildTree ps level = node
    where
        node = Node { left = buildTree (take median sorted) (level+1)
                    , val = sorted !! median
                    , right = buildTree (drop (median+1) sorted) (level+1)
                    , axis = dim
                    }
        dim = level `mod` 2
        sorted = dimSort ps dim
        median = length sorted `div` 2

        dimSort :: Path2D -> Int -> Path2D
        dimSort [] _ = []
        dimSort ps dim
            | dim == 0 = sortByX ps
            | dim == 1 = sortByY ps
            | otherwise = []

removeVec :: KdTree2D Vec2D -> Vec2D -> KdTree2D Vec2D
removeVec Kempty _ = Kempty
removeVec (Node left val right axis) vRem
    | vRem == val                  = buildTree (tree2list left ++ tree2list right) axis
    | dimComp vRem val axis == GT  = Node left                  val (removeVec right vRem) axis
    | otherwise                    = Node (removeVec left vRem) val right axis

dimComp :: Vec2D -> Vec2D -> Int -> Ordering
dimComp v1 v2 dim
    | dim == 0 = compare (x v1) (x v2)
    | dim == 1 = compare (y v1) (y v2)
    | otherwise = EQ

dimVal :: Vec2D -> Int -> Double -- TODO use this in other methods (e.g. dimComp)
dimVal v dim
    | dim == 0 = x v
    | otherwise = y v

kNearest :: KdTree2D Vec2D -> Vec2D -> Int -> Path2D
kNearest Kempty _ _ = []
kNearest _ _ n | n <= 0 = []
kNearest tree p n | n == 1    = [near]
                  | otherwise = near : kNearest newTree p (n-1)
    where
        near = fromJust $ nearest tree p
        newTree = removeVec tree near

        nearest :: KdTree2D Vec2D -> Vec2D -> Maybe Vec2D
        nearest Kempty _ = Nothing
        nearest (Node Kempty val Kempty _) _ = Just val
        nearest (Node left val right axis) p
            | checkRight = find right left
            | otherwise = find left right
            where
                checkRight = dimComp p val axis == GT

                find :: KdTree2D Vec2D -> KdTree2D Vec2D -> Maybe Vec2D
                find treeThis treeOther = Just $ minimumBy (compDistance p) $ [closest] ++ (maybeToList valOther)
                  where
                    valSearch = dimVal p axis
                    valParent = dimVal val axis

                    valOther | mustCheckOther = nearest treeOther p
                             | otherwise      = Nothing

                    mustCheckOther | checkRight = valSearch + bestDistance > valParent
                                   | otherwise  = valSearch - bestDistance < valParent
                    bestDistance = distance p closest
                    closest = minimumBy (compDistance p) $ [val] ++ (maybeToList bestThis)
                    bestThis = nearest treeThis p
