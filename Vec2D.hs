module Vec2D where

data Vec2D = Vec2D {
    x :: Double,
    y :: Double
} deriving (Show, Read)

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
