{-# LANGUAGE FlexibleInstances #-}

module Path2D where

import Vec2D
import Data.List

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
