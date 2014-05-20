module KDtree where

---------------------------------------------------------

import Control.Applicative
import Data.Bits

import qualified Data.List as L

import Boid
import Vec3D

---------------------------------------------------------

data KDtree = Node
                  { kdObject :: Boid
                  , kdAxis   :: Axis
                  , kdRight  :: KDtree
                  , kdLeft   :: KDtree
                  }
              | Leaf
                deriving (Show)

foldKDtree :: (Boid -> a -> a) -> a -> KDtree -> a
foldKDtree func i Leaf = i
foldKDtree func i (Node obj _ r l) =
    let a = foldKDtree func i r
        b = func obj a
    in  foldKDtree func b l

prettyPrint :: KDtree -> String
prettyPrint Leaf = "Leaf\n"
prettyPrint (Node obj axis r l) = "Node {\n\t" ++ "Object: " ++ (show obj) ++ "\n\tAxis: " ++ (show axis) ++
    "\nLeft -\n" ++ (prettyPrint l) ++ "Right -\n" ++ (prettyPrint r) ++ "\n}\n"

---------------------------------------------------------

data Axis = X | Y | Z deriving (Show, Eq, Enum, Bounded)

next :: Axis -> Axis
next Z = X
next x = succ x

coord :: Axis -> (Vec3D -> Float)
coord X = vX
coord Y = vY
coord Z = vZ

---------------------------------------------------------

fromList :: [Boid] -> KDtree
fromList xs = fromList' xs X

fromList' :: [Boid] -> Axis -> KDtree
fromList' [] _    = Leaf
fromList' xs axis = Node
                        { kdObject = m
                        , kdAxis   = axis
                        , kdRight  = fromList' r $ next axis
                        , kdLeft   = fromList' l $ next axis
                        }
    where sorted   = L.sortBy (\a b -> let c = coord axis in compare (c $ bPos a) (c $ bPos b)) xs
          (l, m:r) = L.splitAt (length sorted `div` 2) sorted

toList :: KDtree -> [Boid]
toList = foldKDtree (:) []

nearestNeighbor :: KDtree -> Vec3D -> Maybe Boid
nearestNeighbor tree pos = fst <$> nearestNeighbor' tree pos

nearestNeighbor' :: KDtree -> Vec3D -> Maybe (Boid, Float)
nearestNeighbor' Leaf _ = Nothing
nearestNeighbor' (Node obj axis l r) pt =
    case candidate of
      Just (_, sd) -> if sd > (offset * offset)
                        then cmpPts candidate $ nearestNeighbor' other pt
                        else candidate
      Nothing      -> Nothing
    where offset       = coord axis pt - coord axis (bPos obj)
          (sub, other) = if offset > 0 then (r, l) else (l, r)
          sqDist       = vSqLen $ vSub (bPos obj) pt
          candidate    = cmpPts (Just (obj, sqDist)) (nearestNeighbor' sub pt)
          cmpPts (Just a) (Just b) = if snd a > snd b then Just b else Just a
          cmpPts (Just a) _        = Just a
          cmpPts _        (Just b) = Just b
          cmpPts _        _        = Nothing

kNearestNeighbors :: KDtree -> Vec3D -> Int -> [Boid]
kNearestNeighbors tree pos num = map fst $ kNearestNeighbors' tree pos num

kNearestNeighbors' :: KDtree -> Vec3D -> Int -> [(Boid, Float)]
kNearestNeighbors' Leaf _ _ = []
kNearestNeighbors' (Node obj axis l r) pt num
    | largest > (offset * offset) = take num $ foldl (flip $ L.insertBy cmpPts) newList $ kNearestNeighbors' other pt num
    | otherwise                   = newList
    where offset       = coord axis pt - coord axis (bPos obj)
          (sub, other) = if offset > 0 then (r, l) else (l, r)
          sqDist       = vSqLen $ vSub (bPos obj) pt
          newList      = take num $ L.insertBy cmpPts (obj, sqDist) $ kNearestNeighbors' sub pt num
          largest      = snd $ last newList
          cmpPts a b   = snd a `compare` snd b
