module Octree where

import Vec3D

data Octree a = Node
                  { center :: Vec3D
                  , length :: Float
                  , ftr, ftl, fbr, fbl, btr, btl, bbr, bbl :: Octree a
                  } -- front, back, top, bottom, right, left
              | Leaf
                  { center  :: Vec3D
                  , length :: Float
                  , objects :: [(a, Vec3D)]
                  }
                deriving (Show)

data Octant = FTR | FTL | FBR | FBL | BTR | BTL | BBR | BBL deriving (Show, Eq, Ord, Enum)

emptyOctree :: Vec3D -> Float -> Octree a
emptyOctree c l = Leaf c l []

octreeFold func i (Node _ _ a b c d e f g h) = octreeFold func p h
    where j = octreeFold func i a
          k = octreeFold func j b
          l = octreeFold func k c
          m = octreeFold func l d
          n = octreeFold func m e
          o = octreeFold func n f
          p = octreeFold func o g
octreeFold func i (Leaf _ _ objs) = foldl (func i . fst) objs

getOctant :: Vec3D -> Vec3D -> Octant
getOctant cen pos = toEnum $ (fromEnum right) + (2 * fromEnum top) + (4 * fromEnum front)
    where front = vZ pos > vZ cen
          top   = vY pos > vY cen
          right = vX pos > vX cen

getSubtree :: Octree a -> Octant -> Octree a
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
getSubtree tree octant = tree

count :: Octree a -> Int
count = foldl (\acc _ -> acc + 1) 0

insert :: Octree a -> (a, Vec3D) -> Octree a
insert (Leaf _ _ xs) obj = Leaf $ obj:xs
insert node          obj = insert $ getSubtree node $ getOctant (center node) (snd obj)

splitTree :: Octree a -> Octree a
splitTree (Leaf (Vec3D cx cy cz) l objs) = foldl insert tree objs
    where tree = Node
                   { center = c
                   , length = l
                   , ftr = et rx ty fz, ftl = et lx ty fz
                   , fbr = et rx by fz, fbl = et lx by fz
                   , btr = et rx ty bz, btl = et lx ty bz
                   , bbr = et rx by bz, bbl = et lx by bz
          et x y z = emptyOctree (Vec3D x y z) hl
          hl       = l / 2
          rx       = cx + hl
          lx       = cx - hl
          ty       = cy + hl
          by       = cy - hl
          fz       = cz + hl
          bz       = cz - hl
splitTree tree = tree
