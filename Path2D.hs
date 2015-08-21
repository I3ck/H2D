{-# LANGUAGE FlexibleInstances #-}

module Path2D where

import Vec2D

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
