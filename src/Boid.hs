module Boid where

--------------------------------------------------------------------------------

import Data.List
import Control.Monad (forM)
import qualified Graphics.Rendering.OpenGL as GL

import Vec3D
import Utils

--------------------------------------------------------------------------------

data Boid = Boid
    { bPos :: !Vec3D
    , bVel :: !Vec3D
    , bTar :: !Vec3D
    , bRad :: !Float
    } deriving (Show, Eq)

makeBoids :: (Int, Int, Int) -> (Int, Int, Int) -> Int -> IO [Boid]
makeBoids (lx, ly, lz) (hx, hy, hz) n = forM [1..n] (\_ -> do
        x <- getRandom (rtf lx) (rtf hx) :: IO Float
        y <- getRandom (rtf ly) (rtf hy) :: IO Float
        z <- getRandom (rtf lz) (rtf hz) :: IO Float
        return $ Boid
            { bPos = Vec3D (x, y, z)
            , bVel = vScale (Vec3D (x, y, z)) 0.001
            , bTar = zeroVec
            , bRad = 0.25
            })
    where rtf = realToFrac

updateBoid :: Boid -> [Boid] -> Boid
updateBoid (Boid pos vel tar rad) neighbors =
    let velUpdate = vClamp (foldl (updateVelocity pos) zeroVec neighbors) 0.01
        --tarUpdate = vClamp (updateTarget pos tar) 0.01
        bndUpdate = vClamp (updateBounds pos) 0.015
        newVel    = vScaleTo (vAdd3 vel velUpdate bndUpdate) 0.05
    in  Boid
          { bPos = vAdd pos newVel
          , bVel = newVel
          , bTar = zeroVec
          , bRad = rad
          }

updateBoidRadius :: Boid -> [(Boid, Float)] -> Boid
updateBoidRadius (Boid pos vel tar rad) neighbors =
    let velUpdate = vClamp (foldl (updateVelocityRadius pos) zeroVec neighbors) 0.01
        --tarUpdate = vClamp (updateTarget pos tar) 0.01
        bndUpdate = vClamp (updateBounds pos) 0.015
        newVel    = vScaleTo (vAdd3 vel velUpdate bndUpdate) 0.05
    in  Boid
          { bPos = vAdd pos newVel
          , bVel = newVel
          , bTar = zeroVec
          , bRad = rad
          }

updateVelocity :: Vec3D -> Vec3D -> Boid -> Vec3D
updateVelocity pos vel boid
    | pos == bPos boid = vel
    | otherwise        =
        let deltaP = vSub (bPos boid) pos
            len    = vLen deltaP
        in  vSub (vAdd3 vel deltaP $ vScale (bVel boid) (0.1 / len)) $ vScale deltaP $ 2.5 / len

updateVelocityRadius :: Vec3D -> Vec3D -> (Boid, Float) -> Vec3D
updateVelocityRadius pos vel (boid, radius)
    | pos == bPos boid = vel
    | otherwise        =
        let deltaP = vSub (bPos boid) pos
            len    = radius
        in  vSub (vAdd3 vel deltaP $ vScale (bVel boid) (0.1 / len)) $ vScale deltaP $ 1.5 / len

updateTarget :: Vec3D -> Vec3D -> Vec3D
updateTarget pos tar = vSub tar pos

updateBounds :: Vec3D -> Vec3D
updateBounds pos
    | vSqLen pos > (28 * 28) = vSub zeroVec pos
    | otherwise              = zeroVec

--sortByDistance :: Vec3D -> [Boid] -> [Boid]
--sortByDistance pos = sortBy sortFunc
--    where sortFunc = (\a b -> compare (vSub pos $ bPos a) (vSub pos $ bPos b))

drawBoid :: GL.DisplayList -> Boid -> IO ()
drawBoid dl boid = GL.preservingMatrix $ do
    GL.translate $ toGLVec $ bPos boid
    GL.callList dl

boidDisplayList :: IO GL.DisplayList
boidDisplayList = GL.defineNewList GL.Compile $ do
    GL.colorMaterial GL.$= Just (GL.Front, GL.AmbientAndDiffuse)
    GL.color (GL.Color4 (0.8 :: GL.GLfloat) 0.1 0 1)
    GL.renderPrimitive GL.Quads $ do
        vertex (-0.1)   0.1    0.1
        vertex (-0.1) (-0.1)   0.1
        vertex   0.1  (-0.1)   0.1
        vertex   0.1    0.1    0.1

        vertex   0.1    0.1  (-0.1)
        vertex   0.1  (-0.1) (-0.1)
        vertex (-0.1) (-0.1) (-0.1)
        vertex (-0.1)   0.1  (-0.1)

        vertex (-0.1)   0.1    0.1
        vertex   0.1    0.1    0.1
        vertex   0.1    0.1  (-0.1)
        vertex (-0.1)   0.1  (-0.1)

        vertex (-0.1) (-0.1)   0.1
        vertex (-0.1) (-0.1) (-0.1)
        vertex   0.1  (-0.1) (-0.1)
        vertex   0.1  (-0.1)   0.1

        vertex (-0.1)   0.1  (-0.1)
        vertex (-0.1) (-0.1) (-0.1)
        vertex (-0.1) (-0.1)   0.1
        vertex (-0.1)   0.1    0.1

        vertex   0.1    0.1    0.1
        vertex   0.1  (-0.1)   0.1
        vertex   0.1  (-0.1) (-0.1)
        vertex   0.1    0.1  (-0.1)
