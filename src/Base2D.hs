{-
Copyright 2016 Martin Buck
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

module Base2D where

import Data.List.Split (chunksOf)
import Control.Parallel.Strategies (runEval, rpar, parMap)

--------------------------------------------------------------------------------

pointChunkSize :: Int
pointChunkSize = 1000

chunkParMap :: Int -> (a -> b) -> [a] -> [b]
chunkParMap chunkSize f l = concat $ parMap rpar (map f) (chunksOf chunkSize l)
