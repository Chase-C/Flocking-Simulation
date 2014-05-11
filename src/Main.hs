module Main (main) where

--------------------------------------------------------------------------------

import System.IO
import Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Monad             (unless, when, void)
import Control.Monad.RWS.Strict  (RWST, ask, asks, evalRWST, get, gets, modify, put, liftIO, liftM)
import Foreign.C.String          (CString, newCString)
import Foreign.C.Types           (CInt)
import Foreign.Ptr               (Ptr, nullPtr)
import Foreign.Storable          (peek)
import Data.Bits                 ((.|.))

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.SDL           as SDL

import Boid
import Utils
import Vec3D

--------------------------------------------------------------------------------

data Env = Env
    { envWindow        :: !SDL.Window
    , envZDistClosest  :: !Double
    , envZDistFarthest :: !Double
    , envBoidDispList  :: !GL.DisplayList
    }

data State = State
    { stateRunning         :: !Bool
    , stateWindowWidth     :: !Int
    , stateWindowHeight    :: !Int
    , stateXAngle          :: !Double
    , stateYAngle          :: !Double
    , stateZAngle          :: !Double
    , stateZDist           :: !Double
    , stateMouseDown       :: !Bool
    , stateDragging        :: !Bool
    , stateDragStartX      :: !Int
    , stateDragStartY      :: !Int
    , stateDragStartXAngle :: !Double
    , stateDragStartYAngle :: !Double
    , stateBoids           :: ![Boid]
    }

type Demo = RWST Env () State IO

--------------------------------------------------------------------------------

main :: IO ()
main = do
    let width  = 640
        height = 480

    withFile "log.txt" WriteMode (\h -> return ())

    withWindow width height "Flocking Simulation" $ \win -> do
        void $ SDL.glSetSwapInterval 1

        --GL.position (GL.Light 0) GL.$= GL.Vertex4 5 5 10 0
        --GL.light    (GL.Light 0) GL.$= GL.Enabled
        GL.lighting   GL.$= GL.Disabled
        GL.cullFace   GL.$= Just GL.Back
        GL.depthFunc  GL.$= Just GL.Less
        GL.clearColor GL.$= GL.Color4 0.05 0.05 0.05 1
        GL.normalize  GL.$= GL.Enabled

        boids     <- makeBoids ((-5), (-5), (-5)) (5, 5, 5) 10
        bDispList <- boidDisplayList

        let zDistClosest  = 10
            zDistFarthest = zDistClosest + 20
            zDist         = zDistClosest + ((zDistFarthest - zDistClosest) / 2)
            env = Env
              { envWindow        = win
              , envZDistClosest  = zDistClosest
              , envZDistFarthest = zDistFarthest
              , envBoidDispList  = bDispList
              }
            state = State
              { stateRunning         = True
              , stateWindowWidth     = width
              , stateWindowHeight    = height
              , stateXAngle          = 0
              , stateYAngle          = 0
              , stateZAngle          = 0
              , stateZDist           = zDist
              , stateMouseDown       = False
              , stateDragging        = False
              , stateDragStartX      = 0
              , stateDragStartY      = 0
              , stateDragStartXAngle = 0
              , stateDragStartYAngle = 0
              , stateBoids           = boids
              }
        runDemo env state

--------------------------------------------------------------------------------

-- GLFW-b is made to be very close to the C API, so creating a window is pretty
-- clunky by Haskell standards. A higher-level API would have some function
-- like withWindow.

withWindow :: Int -> Int -> String -> (SDL.Window -> IO ()) -> IO ()
withWindow w h title f = do
    withFile "log.txt" AppendMode (\h -> hPutStrLn h $ "1")
    --i <- SDL.init (SDL.initFlagVideo .|. SDL.initFlagEvents)
    i <- SDL.init (SDL.initFlagEverything)
    withFile "log.txt" AppendMode (\h -> hPutStrLn h $ "2")
    unless (i /= 0) $ do
        cstr <- newCString title
        win  <- SDL.createWindow cstr 100 100 (fi w) (fi h) (SDL.windowFlagOpenGL)
        con  <- SDL.glCreateContext win
        f win
        SDL.glDeleteContext con
        SDL.destroyWindow   win
        SDL.quit
    where fi = fromIntegral

--------------------------------------------------------------------------------

runDemo :: Env -> State -> IO ()
runDemo env state = void $ evalRWST run env state

run :: Demo ()
run = do
    --adjustWindow
    --win <- asks envWindow

    --draw
    --liftIO $ do
    --    GLFW.swapBuffers win
    --    GL.flush  -- not necessary, but someone recommended it
    --    GLFW.pollEvents
    --processEvents

    --state <- get

    --let boids = stateBoids state
    --modify $ \s -> s {
    --    stateBoids = map (\b -> updateBoid b boids) boids
    --    }

    liftIO $ withFile "log.txt" AppendMode (\h -> hPutStrLn h $ "3")

    dragging <- gets stateDragging 
    when dragging $ do
        state <- get
        win   <- asks envWindow
        let sodx  = stateDragStartX      state
            sody  = stateDragStartY      state
            sodxa = stateDragStartXAngle state
            sodya = stateDragStartYAngle state
        let xPtr = nullPtr :: Ptr CInt
            yPtr = nullPtr :: Ptr CInt
        void $ liftIO $ SDL.getMouseState xPtr yPtr
        x <- liftM fromIntegral $ liftIO $ peek xPtr :: Demo Int
        y <- liftM fromIntegral $ liftIO $ peek yPtr :: Demo Int
        let myrot = div (x - sodx) 2
            mxrot = div (y - sody) 2
        put $ state
          { stateXAngle = sodxa + (fromIntegral mxrot)
          , stateYAngle = sodya + (fromIntegral myrot)
          }

    --mt <- liftIO GLFW.getTime

    running <- gets stateRunning
    when running run

processEvents :: Demo ()
processEvents = do
    let evPtr = nullPtr :: Ptr SDL.Event
    val <- liftIO $ SDL.pollEvent evPtr
    case val of
      0 -> return ()
      _ -> do
          ev <- liftIO $ peek evPtr :: Demo SDL.Event
          processEvent ev
          processEvents

processEvent :: SDL.Event -> Demo ()
processEvent ev = do
    case ev of
      (SDL.WindowEvent _ _ _ wev w h) -> do
          when (wev == SDL.windowEventResized) $
            modify $ \s -> s
              { stateWindowWidth  = fi w
              , stateWindowHeight = fi h
              }
          adjustWindow

      (SDL.MouseButtonEvent _ _ _ _ b st _ x y) -> do
          liftIO $ withFile "log.txt" AppendMode (\h -> hPutStrLn h $ show st)
          when (b == SDL.buttonLeft) $ do
              let pressed = (st == 1)
              modify $ \s -> s
                { stateMouseDown = pressed
                }
              unless pressed $
                modify $ \s -> s
                  { stateDragging = False
                  }

      (SDL.MouseMotionEvent _ _ _ _ _ x y _ _) -> do
          state <- get
          when (stateMouseDown state && not (stateDragging state)) $
            put $ state
              { stateDragging        = True
              , stateDragStartX      = fi x
              , stateDragStartY      = fi y
              , stateDragStartXAngle = stateXAngle state
              , stateDragStartYAngle = stateYAngle state
              }

      (SDL.MouseWheelEvent _ _ _ _ _ y) -> do
          env <- ask
          modify $ \s -> s
            { stateZDist =
                let zDist' = stateZDist s + realToFrac (negate $ div y 2)
                in  curb (envZDistClosest env) (envZDistFarthest env) zDist'
            }
          adjustWindow

      (SDL.KeyboardEvent t _ _ _ x (SDL.Keysym k _ _)) ->
          when (t == SDL.eventTypeKeyUp) $ do
            -- Q, Esc: exit
            when (k == SDL.scancodeQ || k == SDL.scancodeEscape) $
                modify $ \s -> s
                  { stateRunning = False
                  }
      _else -> return ()
    where fi = fromIntegral

draw :: Demo ()
draw = do
    env   <- ask
    state <- get
    let xa = stateXAngle state
        ya = stateYAngle state
        za = stateZAngle state
    liftIO $ do
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        GL.preservingMatrix $ do
            GL.rotate (realToFrac xa) xunit
            GL.rotate (realToFrac ya) yunit
            GL.rotate (realToFrac za) zunit
            mapM_ (drawBoid $ envBoidDispList env) $ stateBoids state
      where
        xunit = GL.Vector3 1 0 0 :: GL.Vector3 GL.GLfloat
        yunit = GL.Vector3 0 1 0 :: GL.Vector3 GL.GLfloat
        zunit = GL.Vector3 0 0 1 :: GL.Vector3 GL.GLfloat


adjustWindow :: Demo ()
adjustWindow = do
    state <- get
    let width  = stateWindowWidth  state
        height = stateWindowHeight state
        zDist  = stateZDist        state

    let pos   = GL.Position 0 0
        size  = GL.Size (fromIntegral width) (fromIntegral height)
        h     = fromIntegral height / fromIntegral width :: Double
        znear = 1           :: Double
        zfar  = 40          :: Double
        xmax  = znear * 0.5 :: Double
    liftIO $ do
        GL.viewport   GL.$= (pos, size)
        GL.matrixMode GL.$= GL.Projection
        GL.loadIdentity
        GL.frustum (realToFrac $ -xmax)
                   (realToFrac    xmax)
                   (realToFrac $ -xmax * realToFrac h)
                   (realToFrac $  xmax * realToFrac h)
                   (realToFrac    znear)
                   (realToFrac    zfar)
        GL.matrixMode GL.$= GL.Modelview 0
        GL.loadIdentity
        GL.translate (GL.Vector3 0 0 (negate $ realToFrac zDist) :: GL.Vector3 GL.GLfloat)

--------------------------------------------------------------------------------

curb :: Ord a => a -> a -> a -> a
curb l h x
  | x < l     = l
  | x > h     = h
  | otherwise = x
