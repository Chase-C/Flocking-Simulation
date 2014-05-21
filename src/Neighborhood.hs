module Neighborhood where

---------------------------------------------------------

import Data.Array.IArray

import Boid
import Vec3D

---------------------------------------------------------

type NeighborGrid = Array (Int, Int, Int) Boid

defaultGrid :: Int -> NeighborGrid
defaultGrid n = listArray ((1, 1, 1), (n, n, n)) $ boids
    where boids  = [boid $ Vec3D (r x, r y, r z) | x <- [1..n], y <- [1..n], z <- [1..n]]
          r      = realToFrac
          boid p = Boid
                     { bPos = p
                     , bVel = zeroVec
                     , bTar = zeroVec
                     , bRad = 0
                     }

inBoundsX, inBoundsY, inBoundsZ :: NeighborGrid -> Int -> Bool
inBoundsX grid x = x >= l && x <= u
    where ((l, _, _), (u, _, _)) = bounds grid
inBoundsY grid y = y >= l && y <= u
    where ((_, l, _), (_, u, _)) = bounds grid
inBoundsZ grid z = z >= l && z <= u
    where ((_, _, l), (_, _, u)) = bounds grid

getNeighbors :: NeighborGrid -> (Int, Int, Int) -> [Boid]
getNeighbors grid (x, y, z) = map ((!) grid) [(a, b, c) | a <- xs, b <- ys, c <- zs]
    where xs = axisPts inBoundsX x
          ys = axisPts inBoundsY y
          zs = axisPts inBoundsZ z
          axisPts func n = n : if func grid (n + 1) then
                                 (n + 1) : if inBoundsX grid (n - 1) then [n - 1] else []
                               else [n - 1]
