module Vec2D where

data Vec2D = Vec2D {
    x :: Double,
    y :: Double
} deriving (Show, Read)

--Todo leave data in files, move classes to own files and all instances to the corresponding class file
class Moveable a where
    move :: a -> Vec2D -> a

class Mirrorable a where
    mirrorV :: a -> Double -> a
    mirrorH :: a -> Double -> a
    mirrorP :: a -> Vec2D -> a

class Rotateable a where
    rotate :: a -> Double -> Vec2D -> a

instance Moveable Vec2D where
    move (Vec2D a b) (Vec2D x y) = Vec2D (a+x) (b+y)

instance Mirrorable Vec2D where
    mirrorV (Vec2D x y) a           = Vec2D (2 * a - x) y
    mirrorH (Vec2D x y) a           = Vec2D x (2 * a - y)
    mirrorP (Vec2D x y) (Vec2D a b) = Vec2D (2 * a - x) (2 * b - y)

instance Rotateable Vec2D where
    rotate (Vec2D x y) rad (Vec2D centerX centerY) = Vec2D newX newY
        where
            newX = centerX + cos rad * (x - centerX) - sin rad * (y - centerY)
            newY = centerY + sin rad * (x - centerY) + cos rad * (y - centerY)

--------------------------------------------------------------------------------

absolute :: Vec2D -> Double
absolute (Vec2D x y) = sqrt ((x*x) + (y*y))

--------------------------------------------------------------------------------

phi :: Vec2D -> Double
phi (Vec2D x y) = atan2 y x

--------------------------------------------------------------------------------

distance :: Vec2D -> Vec2D -> Double
distance (Vec2D x y) (Vec2D a b) = sqrt( (x-a)**2 + (y-b)**2 )

--------------------------------------------------------------------------------

slope :: Vec2D -> Vec2D -> Double
slope (Vec2D x y) (Vec2D a b) = (b - y) / (a - x)

--------------------------------------------------------------------------------

radTo :: Vec2D -> Vec2D -> Double
radTo (Vec2D x y) (Vec2D a b) = atan2 (b - y) (a - x)

--------------------------------------------------------------------------------

center :: Vec2D -> Vec2D -> Vec2D
center (Vec2D x y) (Vec2D a b) = Vec2D newX newY
    where
        newX = x + (a - x) / 2.0
        newY = y + (b - y) / 2.0

--------------------------------------------------------------------------------

equal :: Vec2D -> Vec2D -> Bool
equal (Vec2D x y) (Vec2D a b) = res
    where
        res = x == a && y == b

similar :: Vec2D -> Vec2D -> Double -> Bool
similar v1 v2 delta = res <= delta
    where
        res = distance v1 v2

--------------------------------------------------------------------------------

parse :: String -> Vec2D
parse line = Vec2D{ x = d1, y = d2 }
    where
        d1 = head dbls
        d2 = head dbls
        dbls = map read  (words line)
