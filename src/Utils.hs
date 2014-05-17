module Utils where

import System.Random
import Foreign.C.Types           (CInt)
import Foreign.Marshal.Alloc     (malloc, free)
import Foreign.Ptr               (Ptr)
import Foreign.Storable          (peek)

import qualified Graphics.UI.SDL           as SDL
import qualified Graphics.Rendering.OpenGL as GL

import Vec3D

getRandom :: (Random a) => a -> a -> IO a
getRandom l u = do
    seed <- getStdGen
    let (n, s) = randomR (l, u) seed
    setStdGen s
    return n

getMousePos :: IO (Int, Int)
getMousePos = do
    xPtr <- malloc :: IO (Ptr CInt)
    yPtr <- malloc :: IO (Ptr CInt)
    _ <- SDL.getMouseState xPtr yPtr
    x <- peek xPtr
    y <- peek yPtr
    free xPtr
    free yPtr
    return (fromIntegral x, fromIntegral y)

toGLVec :: Vec3D -> GL.Vector3 GL.GLfloat
toGLVec (Vec3D (x, y, z)) = GL.Vector3 (realToFrac x) (realToFrac y) (realToFrac z)

vertex :: Float -> Float -> Float -> IO ()
vertex x y z = GL.vertex $ GL.Vertex3 (realToFrac x :: GL.GLfloat) (realToFrac y) (realToFrac z)
