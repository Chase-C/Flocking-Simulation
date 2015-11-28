module Shader where

import Prelude hiding ((<*))
import Graphics.GPipe
import Linear

import Utils

type BoidShader os c ds = PrimitiveArray Triangles ((B3 Float, B3 Float), (B3 Float, B3 Float)) -> Render os (ContextFormat c ds) ()

transformStream :: (IfB a, OrdB a, Floating a) => M44 a -> ((V3 a, V3 a), (V3 a, V3 a)) -> (V4 a, V3 a)
transformStream mvp ((vert, norm), (pos, dir)) = (pos', color)
    where
        normDir      = vNorm dir
        axis         = vNorm $ cross (V3 0 0 1) normDir
        aAxis        = vNorm $ cross normDir axis
        rotationMat  = transpose $ V3 axis aAxis normDir
        transformMat = mvp !*! mkTransformationMat rotationMat pos
        pos'         = transformMat !* makeV4 vert
        rNorm        = rotationMat  !* norm
        (V4 _ _ d _) = mvp          !* makeV4 pos
        color        = makeColor rNorm d

{-# INLINE makeV4 #-}
makeV4 :: Num a => V3 a -> V4 a
makeV4 (V3 x y z) = V4 x y z 1

{-# INLINE interpolate #-}
interpolate :: (IfB a, OrdB a, Floating a) => V3 a -> V3 a -> a -> V3 a
interpolate v1 v2 t = ((1 - t') *^ v1) ^+^ (t' *^ v2)
    where t' = sClamp 0 1 t

{-# INLINE sClamp #-}
sClamp :: (IfB a, OrdB a, Floating a) => a -> a -> a -> a
sClamp lower upper val = minB upper (maxB lower val)

{-# INLINE makeColor #-}
makeColor :: (IfB a, OrdB a, Floating a) => V3 a -> a -> V3 a
makeColor normal depth = color'
    where lightVal = normal `dot` V3 0 1 0
          cLight x = (0.6 + (x / 1.75)) *^ V3 0.2 0.4 0.8
          cDark  x = (0.9 + (x / 1.75)) *^ V3 0.2 0.1 0.5
          fadeVal  = (100 - depth) / 80
          color    = caseB lightVal
                         [ ((>*   0.1),  cLight lightVal)
                         , ((<* (-0.1)), cDark  lightVal)
                         ]
                         (interpolate (cDark (-0.1)) (cLight 0.1) ((0.1 + lightVal) * 5))
          color'   = interpolate (V3 0.010 0.015 0.035) color fadeVal

{-# INLINE getColorDepth #-}
getColorDepth :: V3 FFloat -> RasterizedInfo -> (V3 FFloat, FFloat)
getColorDepth color info = (color, depth)
    where (V4 _ _ depth _) = rasterizedFragCoord info
