#H2D
A Haskell library for paths defined by points within the 2d space
##Version 0.0.3

##data types

```haskell
Vec2D --Point or Vector in 2D space
Line2D --two Points or Vectors
Path2D --n Points
```

###path constructors
```
createInvolutCircle
createArc
createEllipse
createRectangle
```

###methods
```
convexHull
removeAbove
removeBelow
removeRightOf
removeLeftOf
removeCloserThan
removeFurtherThan
boundingBox
intersections
...and many more
```

##contribute  
If you find any bugs, feel free to open an issue  
If you'd like other Paths than Arc etc. open an issue  

##license
LGPL (see `LICENSE`)
