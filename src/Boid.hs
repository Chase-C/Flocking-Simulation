module Boid where

--------------------------------------------------------------------------------

import Control.Monad (forM)
import qualified Graphics.Rendering.OpenGL as GL

import Vec3D
import Utils

--------------------------------------------------------------------------------

data Boid = Boid {
    bPos :: !Vec3D,
    bVel :: !Vec3D,
    bRad :: !Float
  } deriving (Show, Eq)

makeBoids :: (Int, Int, Int) -> (Int, Int, Int) -> Int -> IO [Boid]
makeBoids (lx, ly, lz) (hx, hy, hz) n = forM [1..n] (\_ -> do
        x <- fmap fromIntegral $ getRandom lx hx
        y <- fmap fromIntegral $ getRandom ly hy
        z <- fmap fromIntegral $ getRandom lz hz
        return $ Boid {
            bPos = Vec3D (x, y, z),
            bVel = zeroVec,
            bRad = 0.25
          })

updateBoid :: Boid -> [Boid] -> Boid
updateBoid (Boid pos vel rad) neighbors =
    let velUpdate = foldl (updateVelocity pos) zeroVec neighbors
        newVel    = vClamp (vAdd vel $ vClamp velUpdate 0.01) 0.02
    in  Boid {
            bPos = vAdd pos newVel,
            bVel = newVel,
            bRad = rad
          }

updateVelocity :: Vec3D -> Vec3D -> Boid -> Vec3D
updateVelocity pos vel boid =
    let deltaP = vSub (bPos boid) pos
        len    = vLen deltaP
    in  vSub (vAdd vel deltaP) $ vScale deltaP $ 0.8 / len

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
