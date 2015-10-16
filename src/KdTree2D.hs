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

module KdTree2D where

import Debug.Trace

import Data.List (minimumBy)
import Data.Maybe (fromJust, maybeToList)

import Vec2D
import Path2D

data KdTree2D v  = Node { left   :: KdTree2D v
                        , val    :: v
                        , right  :: KdTree2D v
                        , axis   :: Int
                        }
                 | Kempty
    deriving (Eq, Ord, Show)

--instance Show (KdTree2D Vec2D) where
    --show Kempty = "[]"
    --show (Node left val right) = "(" ++ show left ++ " " ++ show val ++ " " ++ show right ++ ")"

instance Foldable KdTree2D where
    foldr f init Kempty = init
    foldr f init (Node left val right axis) = foldr f z3 left
        where
            z3 = f val z2
            z2 = foldr f init right

instance Functor KdTree2D where
    fmap _ Kempty = Kempty
    fmap f (Node left val right axis) = Node (fmap f left) (f val) (fmap f right) axis

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
    | vRem == val                                                = buildTree (tree2list left ++ tree2list right) axis
    | dimComp vRem val axis == LT || dimComp vRem val axis == EQ = Node (removeVec left vRem) val right axis --TODO simplify condition
    | otherwise                                                  = Node left                  val (removeVec right vRem) axis

dimComp :: Vec2D -> Vec2D -> Int -> Ordering
dimComp v1 v2 dim
    | dim == 0 && (x v1) > (x v2) = GT
    | dim == 0 && (x v1) < (x v2) = LT
    | dim == 1 && (y v1) > (y v2) = GT
    | dim == 1 && (y v1) < (y v2) = LT
    | otherwise = EQ

dimVal :: Vec2D -> Int -> Double -- TODO use this in other methods (e.g. dimComp)
dimVal v dim
    | dim == 0 = x v
    | dim == 1 = y v

kNearest :: KdTree2D Vec2D -> Vec2D -> Int -> Path2D
kNearest Kempty _ _ = []
kNearest _ _ n | n <= 0 = []
kNearest tree p n = near : kNearest newTree p (n-1)
    where
        near = fromJust $ nearest tree p
        newTree = removeVec tree near

        nearest :: KdTree2D Vec2D -> Vec2D -> Maybe Vec2D
        nearest Kempty _ = Nothing
        nearest (Node Kempty val Kempty axis) _ = Just val
        nearest (Node left val right axis) p
            | dimComp p val axis == LT || dimComp p val axis == EQ = find left right --TODO simplify condition
            | otherwise = find right left
            where
                find :: KdTree2D Vec2D -> KdTree2D Vec2D -> Maybe Vec2D
                find t1 t2 = Just $ minimumBy (compDistance p) $ ps2
                    where
                        dimP = dimVal p axis
                        dimV = dimVal val axis
                        ps1 = case nearest t1 p of
                                  Nothing  -> [val]
                                  Just res -> [res, val]
                        ps2 | intersects = ps1 ++ maybeToList (nearest t2 p)
                            | otherwise  = ps1
                        intersects = (dimP - dimV)^2 <= sqrDistance p val
