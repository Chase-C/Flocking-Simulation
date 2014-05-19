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
                  , len :: Float
                  , ftr, ftl, fbr, fbl, btr, btl, bbr, bbl :: Octree
                  } -- front, back, top, bottom, right, left
              | Leaf
                  { center  :: Vec3D
                  , len     :: Float
                  , objects :: [Boid]
                  }
                deriving (Show)

data Octant = FTR | FTL | FBR | FBL | BTR | BTL | BBR | BBL deriving (Show, Eq, Ord, Enum)

---------------------------------------------------------

emptyOctree :: Vec3D -> Float -> Octree
emptyOctree c l = Leaf c l []

---------------------------------------------------------

octreeMap :: (Boid -> Boid) -> Octree -> Octree
octreeMap func tree = insertList (emptyOctree (center tree) (len tree)) $ map func boids
    where boids = flattenTree tree

octreeFold :: (a -> Boid -> a) -> a -> Octree -> a
octreeFold func i (Node _ _ a b c d e f g h) = octreeFold func p h
    where j = octreeFold func i a
          k = octreeFold func j b
          l = octreeFold func k c
          m = octreeFold func l d
          n = octreeFold func m e
          o = octreeFold func n f
          p = octreeFold func o g
octreeFold func i (Leaf _ _ objs) = foldl (\acc -> func acc) i objs

---------------------------------------------------------

prettyPrint :: Octree -> String
prettyPrint (Node cen l a b c d e f g h) = "Node {\n\tcenter: " ++ (show cen) ++ "\n\tlength: " ++ (show l) ++ "\n" ++
    (concat $ L.intersperse "\n" $ map prettyPrint [a, b, c, d, e, f, g, h]) ++ "\n}"
prettyPrint (Leaf cen l objs) = "Leaf {\n\tcenter: " ++ (show cen) ++ "\n\tlength: " ++ (show l) ++ "\n\t" ++
    (concat $ L.intersperse "\n\t" $ map show objs) ++ "\n}"

---------------------------------------------------------

getOctant :: Vec3D -> Vec3D -> Octant
getOctant cen pos = toEnum $ (fromEnum right) + (2 * fromEnum top) + (4 * fromEnum front)
    where front = vZ pos < vZ cen
          top   = vY pos < vY cen
          right = vX pos < vX cen

getSubtree :: Octree -> Octant -> Octree
getSubtree (Node _ _ a b c d e f g h) octant =
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
replaceSubtree (Node cen l a b c d e f g h) octant subtree =
    case octant of
      FTR -> Node cen l subtree b c d e f g h
      FTL -> Node cen l a subtree c d e f g h
      FBR -> Node cen l a b subtree d e f g h
      FBL -> Node cen l a b c subtree e f g h                                
      BTR -> Node cen l a b c d subtree f g h
      BTL -> Node cen l a b c d e subtree g h
      BBR -> Node cen l a b c d e f subtree h
      BBL -> Node cen l a b c d e f g subtree
replaceSubtree tree _ _ = tree

flattenTree :: Octree -> [Boid]
flattenTree tree = octreeFold (\xs obj -> obj:xs) [] tree

count :: Octree -> Int
count = octreeFold (\acc _ -> acc + 1) 0

insert :: Octree -> Boid -> Octree
insert (Leaf cen l xs) obj = Leaf cen l $ obj:xs
insert node            obj = replaceSubtree node octant $ insert (getSubtree node octant) obj
    where octant = getOctant (center node) (bPos obj)

insertList :: Octree -> [Boid] -> Octree
insertList = foldl insert

splitTree :: Octree -> Octree
splitTree (Leaf c@(Vec3D (cx, cy, cz)) l objs) = foldl insert tree objs
    where tree = Node
                   { center = c
                   , len = l
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
splitWith (Node cen len i j k l m n o p) f = Node cen len (s i) (s j) (s k) (s l) (s m) (s n) (s o) (s p)
    where s tree = splitWith tree f
splitWith tree func 
    | func tree = splitWith (splitTree tree) func
    | otherwise = tree

getNearObjects :: Octree -> Vec3D -> [Boid]
getNearObjects (Leaf _ _ objs) _ = objs
getNearObjects node pos          = getNearObjects subtree pos
    where subtree = getSubtree node $ getOctant (center node) pos

-- Optimize the shit out of this
-- Idea: Only cast 7 (max) vectors toward other octants
--       instead of 27
--getRadiusObjects' :: Octree -> [Vec3D] -> Float -> [Boid]
--getRadiusObjects' (Leaf _ _ objs) (p:_) r = filter (\obj -> (r * r) > (vSqLen $ vSub p $ bPos obj)) objs
--getRadiusObjects' node pts r              = concat $ map (\t -> getRadiusObjects' t pts r) subtrees
--    where subtrees = map (getSubtree node) $ if r > len node
--                                               then map toEnum [0..7]
--                                               else L.nub $ map (getOctant (center node)) pts
--
--getRadiusObjects :: Octree -> Vec3D -> Float -> [Boid]
--getRadiusObjects tree pos r = getRadiusObjects' tree pts r
--    where pts     = pos:foldl (\ps off -> (vAdd pos off):(vSub pos off):ps) [] offsets
--          -- Whoops, these aren't all normalized
--          offsets = [v  (r,0,0),  v  ( 0,r,0), v  (0,0, r), vs (1, 1,0), vs ( 0,1,1), vs (1,0, 1),
--                     vs (1,-1,0), vs (0,1,-1), vs (1,0,-1), vs (1, 1,1), vs (-1,1,1), vs (1,1,-1), vs (1,-1,1)]
--          v       = Vec3D
--          vs pt   = vScale (v pt) r

-----------------------------------------------
-- Testing
-----------------------------------------------

xOppOctant, yOppOctant, zOppOctant :: Octant -> Octant
xOppOctant octant = toEnum $ xor (fromEnum octant) 1
yOppOctant octant = toEnum $ xor (fromEnum octant) 2
zOppOctant octant = toEnum $ xor (fromEnum octant) 4

getRadiusObjects :: Octree -> Vec3D -> Float -> [Boid]
getRadiusObjects (Leaf _ l objs) pos r
    | r > l     = objs
    | otherwise = filter (\obj -> (r * r) > (vSqLen $ vSub pos $ bPos obj)) objs
getRadiusObjects node pos r = concat $ map (\o -> getRadiusObjects (getSubtree node o) pos r) others
    where octant = getOctant (center node) pos
          others = if r > len node
                     then map toEnum [0..7] :: [Octant]
                     else [x . y . z | z <- zList, y <- yList, x <- xList] <*> [octant]
          xList  = id : if r > abs ((vX pos) - (vX $ center node)) then [xOppOctant] else []
          yList  = id : if r > abs ((vY pos) - (vY $ center node)) then [yOppOctant] else []
          zList  = id : if r > abs ((vZ pos) - (vZ $ center node)) then [zOppOctant] else []
