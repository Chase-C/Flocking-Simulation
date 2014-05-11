module Utils where

import System.Random
import qualified Graphics.Rendering.OpenGL as GL

import Vec3D

getRandom :: Int -> Int -> IO Int
getRandom l u = do
    seed <- getStdGen
    let (n, s) = randomR (l, u) seed
    setStdGen s
    return n

toGLVec :: Vec3D -> GL.Vector3 GL.GLfloat
toGLVec (Vec3D (x, y, z)) = GL.Vector3 (realToFrac x) (realToFrac y) (realToFrac z)

vertex :: Float -> Float -> Float -> IO ()
vertex x y z = GL.vertex $ GL.Vertex3 (realToFrac x :: GL.GLfloat) (realToFrac y) (realToFrac z)
