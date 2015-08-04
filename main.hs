main = do
    putStrLn "hello"
    let p1 = Vec2D {x = 1.0, y=3.0}
    let p2 = Vec2D {x = 2.0, y=7.0}
    let p3 = p1 `add` p2
    print p3


data Vec2D = Vec2D {
    x :: Double,
    y :: Double
} deriving (Show)

add :: Vec2D -> Vec2D -> Vec2D
(Vec2D a b) `add` (Vec2D x y) = Vec2D (a+x) (b+y)
