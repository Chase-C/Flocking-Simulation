module Octree where

---------------------------------------------------------

import Control.Applicative
import Data.Bits

import qualified Data.List as L

import Vec3D
import Boid

---------------------------------------------------------

data Octree = Node
                  { center :: Vec3D
                  , len    :: Float
                  , count  :: Int
                  , ftr, ftl, fbr, fbl, btr, btl, bbr, bbl :: Octree
                  } -- front, back, top, bottom, right, left
            | Leaf
                  { center  :: Vec3D
                  , len     :: Float
                  , count   :: Int
                  , objects :: [Boid]
                  }
                deriving (Show)

data Octant = FTR | FTL | FBR | FBL | BTR | BTL | BBR | BBL deriving (Show, Eq, Ord, Enum)

---------------------------------------------------------

emptyOctree :: Vec3D -> Float -> Octree
emptyOctree c l = Leaf c l 0 []

fromList :: [Boid] -> Vec3D -> Float -> Octree
fromList boids c l = foldl insert (emptyOctree c l) boids

---------------------------------------------------------

octreeMap :: (Boid -> Boid) -> Octree -> Octree
octreeMap func tree = insertList (emptyOctree (center tree) (len tree)) $ map func boids
    where boids = flattenTree tree

octreeMapM_ :: (Boid -> IO a) -> Octree -> IO ()
octreeMapM_ func (Leaf _ _ _ objs) = mapM_ func objs
octreeMapM_ func (Node _ _ _ a b c d e f g h) = do
    octreeMapM_ func a
    octreeMapM_ func b
    octreeMapM_ func c
    octreeMapM_ func d
    octreeMapM_ func e
    octreeMapM_ func f
    octreeMapM_ func g
    octreeMapM_ func h
    return ()

octreeFold :: (a -> Boid -> a) -> a -> Octree -> a
octreeFold func i (Node _ _ _ a b c d e f g h) = octreeFold func p h
    where j = octreeFold func i a
          k = octreeFold func j b
          l = octreeFold func k c
          m = octreeFold func l d
          n = octreeFold func m e
          o = octreeFold func n f
          p = octreeFold func o g
octreeFold func i (Leaf _ _ _ objs) = foldl func i objs

---------------------------------------------------------

prettyPrint :: Octree -> String
prettyPrint (Node cen l cnt a b c d e f g h) = "Node {\n\tcenter: " ++ (show cen) ++ "\n\tlength: " ++ (show l) ++
    "\n\tcount: " ++ (show cnt) ++ "\n" ++ (concat $ L.intersperse "\n" $ map prettyPrint [a, b, c, d, e, f, g, h]) ++ "\n}"
prettyPrint (Leaf cen l cnt objs) = "Leaf {\n\tcenter: " ++ (show cen) ++ "\n\tlength: " ++ (show l) ++ 
    "\n\tcount: " ++ (show cnt) ++ "\n" ++ (concat $ L.intersperse "\n\t" $ map show objs) ++ "\n}"

---------------------------------------------------------

getOctant :: Vec3D -> Vec3D -> Octant
getOctant cen pos = toEnum $ (fromEnum right) + (2 * fromEnum top) + (4 * fromEnum front)
    where front = vZ pos < vZ cen
          top   = vY pos < vY cen
          right = vX pos < vX cen

getSubtree :: Octree -> Octant -> Octree
getSubtree (Node _ _ _ a b c d e f g h) octant =
    case octant of
      FTR -> a
      FTL -> b
      FBR -> c
      FBL -> d
      BTR -> e
      BTL -> f
      BBR -> g
      BBL -> h
getSubtree tree _ = tree

replaceSubtree :: Octree -> Octant -> Octree -> Octree
replaceSubtree t@(Node cen l cnt a b c d e f g h) octant subtree =
    case octant of
      FTR -> Node cen l nCnt subtree b c d e f g h
      FTL -> Node cen l nCnt a subtree c d e f g h
      FBR -> Node cen l nCnt a b subtree d e f g h
      FBL -> Node cen l nCnt a b c subtree e f g h                                
      BTR -> Node cen l nCnt a b c d subtree f g h
      BTL -> Node cen l nCnt a b c d e subtree g h
      BBR -> Node cen l nCnt a b c d e f subtree h
      BBL -> Node cen l nCnt a b c d e f g subtree
    where nCnt = cnt - (count $ getSubtree t octant) + (count subtree)
replaceSubtree tree _ _ = tree

flattenTree :: Octree -> [Boid]
flattenTree = octreeFold (flip (:)) []

insert :: Octree -> Boid -> Octree
insert (Leaf cen l cnt xs) obj = Leaf cen l (cnt + 1) $ obj:xs
insert node                obj = replaceSubtree node octant $ insert (getSubtree node octant) obj
    where octant = getOctant (center node) (bPos obj)

insertList :: Octree -> [Boid] -> Octree
insertList = foldl insert

splitTree :: Octree -> Octree
splitTree (Leaf c@(Vec3D (cx, cy, cz)) l cnt objs) = foldl insert tree objs
    where tree = Node
                   { center = c
                   , len    = l
                   , count  = cnt
                   , ftr = et rx ty fz, ftl = et lx ty fz
                   , fbr = et rx by fz, fbl = et lx by fz
                   , btr = et rx ty bz, btl = et lx ty bz
                   , bbr = et rx by bz, bbl = et lx by bz
                   }
          et x y z = emptyOctree (Vec3D (x, y, z)) hl
          hl       = l / 2
          rx       = cx + hl
          lx       = cx - hl
          ty       = cy + hl
          by       = cy - hl
          fz       = cz + hl
          bz       = cz - hl
splitTree tree = tree

splitWith :: Octree -> (Octree -> Bool) -> Octree
splitWith (Node cen len cnt i j k l m n o p) f = Node cen len cnt (s i) (s j) (s k) (s l) (s m) (s n) (s o) (s p)
    where s tree = splitWith tree f
splitWith tree func 
    | func tree = splitWith (splitTree tree) func
    | otherwise = tree

getNearObjects :: Octree -> Vec3D -> [Boid]
getNearObjects (Leaf _ _ _ objs) _ = objs
getNearObjects node pos            = getNearObjects subtree pos
    where subtree = getSubtree node $ getOctant (center node) pos

xOppOctant, yOppOctant, zOppOctant :: Octant -> Octant
xOppOctant octant = toEnum $ xor (fromEnum octant) 1
yOppOctant octant = toEnum $ xor (fromEnum octant) 2
zOppOctant octant = toEnum $ xor (fromEnum octant) 4

getRadiusObjects :: Octree -> Vec3D -> Float -> [Boid]
getRadiusObjects (Leaf _ l _ objs) pos r
    | r > l     = objs
    | otherwise = filter (\obj -> (r * r) > (vSqLen $ vSub pos $ bPos obj)) objs
getRadiusObjects node pos r = concat . (map (\t -> getRadiusObjects t pos r)) $ intersectingSubtrees node pos r

-- Return True iff the sphere around the given position exceeds the bounds of
-- the given Octree.
{-# INLINE inBounds #-}
inBounds :: Octree -> Vec3D -> Float -> Bool
inBounds tree pos rad = lX && lY && lZ && uX && uY && uZ
    where Vec3D (x, y, z) = vSub pos $ center tree
          hl = len tree / 2
          lX = -hl < x - rad
          lY = -hl < y - rad
          lZ = -hl < z - rad
          uX =  hl > x + rad
          uY =  hl > y + rad
          uZ =  hl > z + rad

-- Return a list of the subtrees intersecting with the given bounding sphere
-- Note: The last subtree in the list is always the the one containing the point
{-# INLINE intersectingSubtrees #-}
intersectingSubtrees :: Octree -> Vec3D -> Float -> [Octree]
intersectingSubtrees l@(Leaf {}) _ _ = return l
intersectingSubtrees node p@(Vec3D (px, py, pz)) rad = map (getSubtree node) octants
    where octant  = getOctant c p
          octants = if rad > abs (pz - cz) then foldr (\o zs -> (zOppOctant o):o:zs) [] tmpY else tmpY
          tmpY    = if rad > abs (py - cy) then foldr (\o ys -> (yOppOctant o):o:ys) [] tmpX else tmpX
          tmpX    = if rad > abs (px - cx) then (xOppOctant octant):[octant] else [octant]
          c@(Vec3D (cx, cy, cz)) = center node

kNearestNeighbors :: Octree -> Vec3D -> Int -> Float -> [(Boid, Float)]
kNearestNeighbors (Leaf _ _ _ objs) pos k maxR =
    take k $ L.sortBy sortByDist $ filter filtFunc $ map (getObjDist pos) objs
    where filtFunc (_, rad)        = rad < maxR
kNearestNeighbors node pos k maxR
    | inBounds subtree pos topR && length nearest >= k = nearest
    | otherwise = take k $ foldl combineNeighbors nearest $ getOtherNeighbors node pos k topR
    where subtree = getSubtree node (getOctant (center node) pos)
          nearest = kNearestNeighbors subtree pos k maxR
          topR    = if length nearest >= k then snd $ last nearest else maxR

{-# INLINE getOtherNeighbors #-}
getOtherNeighbors :: Octree -> Vec3D -> Int -> Float -> [[(Boid, Float)]]
getOtherNeighbors tree pos k rad =
    map (\t -> kNearestNeighbors t pos k rad) $ init $ intersectingSubtrees tree pos rad

{-# INLINE combineNeighbors #-}
combineNeighbors :: [(Boid, Float)] -> [(Boid, Float)] -> [(Boid, Float)]
combineNeighbors xs [] = xs
combineNeighbors [] ys = ys
combineNeighbors (x@(_, rx):xs) (y@(_, ry):ys) =
    if rx > ry
      then y : combineNeighbors (x:xs) ys
      else x : combineNeighbors xs (y:ys)

{-# INLINE getObjDist #-}
getObjDist :: Vec3D -> Boid -> (Boid, Float)
getObjDist pos obj = (obj, vDist (bPos obj) pos)

{-# INLINE sortByDist #-}
sortByDist :: (Boid, Float) -> (Boid, Float) -> Ordering
sortByDist (_, r1) (_, r2) = r1 `compare` r2
