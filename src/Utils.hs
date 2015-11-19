module Utils where

import System.Random
--import Foreign.C.Types           (CInt)
--import Foreign.Marshal.Alloc     (malloc, free)
--import Foreign.Ptr               (Ptr)
--import Foreign.Storable          (peek)

import Linear

getRandom :: (Random a) => a -> a -> IO a
getRandom l u = do
    seed <- getStdGen
    let (n, s) = randomR (l, u) seed
    setStdGen s
    return n

{-# INLINE vDot #-}
{-# INLINE vDist #-}
vDot, vDist :: (Floating a, Eq a) => V3 a -> V3 a -> a
vDot  (V3 x1 y1 z1) (V3 x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)
vDist (V3 x1 y1 z1) (V3 x2 y2 z2) = sqrt $ (x * x) + (y * y) + (z * z)
    where x = x2 - x1
          y = y2 - y1
          z = z2 - z1

{-# INLINE vLen #-}
{-# INLINE vSqLen #-}
vLen, vSqLen :: (Floating a, Eq a) => V3 a -> a
vSqLen (V3 x y z) = (x * x) + (y * y) + (z * z)
vLen              = sqrt . vSqLen

{-# INLINE vScale #-}
{-# INLINE vScaleTo #-}
{-# INLINE vClamp #-}
vScale, vScaleTo, vClamp :: (Floating a, Eq a, Ord a) => V3 a -> a -> V3 a
vScale (V3 0 0 0) _ = zero
vScale (V3 x y z) n = V3 (n * x) (n * y) (n * z)
vClamp (V3 0 0 0) _ = zero
vClamp vec        n = if sqlen > (n * n) then vec ^* (n / (sqrt sqlen)) else vec
    where sqlen = vSqLen vec
vScaleTo vec      n = vec ^* (n / (vLen vec))

{-# INLINE vNorm #-}
vNorm :: (Floating a, Eq a) => V3 a -> V3 a
vNorm vec = vec ^/ (vLen vec)
