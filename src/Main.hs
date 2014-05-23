module Main (main) where

--------------------------------------------------------------------------------

import System.IO
import Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Monad             (unless, when, void)
import Control.Monad.RWS.Strict  (RWST, ask, asks, evalRWST, get, gets, modify, put, liftIO, liftM)
import Foreign.C.String          (CString, newCString, peekCString)
import Foreign.C.Types           (CInt)
import Foreign.Marshal.Alloc     (malloc, free)
import Foreign.Ptr               (Ptr)
import Foreign.Storable          (peek)
import Data.Bits                 ((.|.))
import Data.Word                 (Word32)
import Data.Array.IArray         ((//), assocs, elems)
import Data.List                 (sortBy)

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.SDL           as SDL

import Boid
import Utils
import Vec3D

import qualified Octree as O

--------------------------------------------------------------------------------

data Env = Env
    { envWindow        :: !SDL.Window
    , envFPS           :: !Int
    , envZDistClosest  :: !Double
    , envZDistFarthest :: !Double
    , envBoidDispList  :: !GL.DisplayList
    }

data State = State
    { stateRunning         :: !Bool
    , stateGameTime        :: !Word32
    , stateDt              :: !Word32
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
    , stateOctree          :: !O.Octree
    }

type Demo = RWST Env () State IO

--------------------------------------------------------------------------------

main :: IO ()
main = do
    let width  = 800
        height = 600

    --withFile "log.txt" WriteMode (\h -> return ())

    withWindow width height "Flocking Simulation" $ \win -> do
        void $ SDL.glSetSwapInterval 1

        --GL.position (GL.Light 0) GL.$= GL.Vertex4 5 5 10 0
        --GL.light    (GL.Light 0) GL.$= GL.Enabled
        GL.lighting   GL.$= GL.Disabled
        GL.cullFace   GL.$= Just GL.Back
        GL.depthFunc  GL.$= Just GL.Less
        GL.clearColor GL.$= GL.Color4 0.05 0.05 0.05 1
        GL.normalize  GL.$= GL.Enabled

        boids     <- makeBoids ((-20), (-20), (-20)) (20, 20, 20) 500
        bDispList <- boidDisplayList

        let zDistClosest  = 10
            zDistFarthest = zDistClosest + 40
            zDist         = zDistClosest + ((zDistFarthest - zDistClosest) / 2)
            env = Env
              { envWindow        = win
              , envFPS           = 60
              , envZDistClosest  = zDistClosest
              , envZDistFarthest = zDistFarthest
              , envBoidDispList  = bDispList
              }
            state = State
              { stateRunning         = True
              , stateGameTime        = 0
              , stateDt              = 0
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
              , stateBoids           = []
              , stateOctree          = O.splitWith (O.fromList boids (Vec3D (0, 0, 0)) 64) ((> 8) . O.count)
              }
        runDemo env state

--------------------------------------------------------------------------------

-- GLFW-b is made to be very close to the C API, so creating a window is pretty
-- clunky by Haskell standards. A higher-level API would have some function
-- like withWindow.

withWindow :: Int -> Int -> String -> (SDL.Window -> IO ()) -> IO ()
withWindow w h title f = do
    --withFile "log.txt" AppendMode (\h -> hPutStrLn h $ "1")
    i <- SDL.init (SDL.initFlagVideo .|. SDL.initFlagEvents .|. SDL.initFlagTimer)
    if i == 0
      then do
        cstr <- newCString title
        win  <- SDL.createWindow cstr 100 100 (fi w) (fi h) (SDL.windowFlagOpenGL)
        con  <- SDL.glCreateContext win
        f win
        SDL.glDeleteContext con
        SDL.destroyWindow   win
        SDL.quit
      else do
        err <- SDL.getError >>= peekCString
        withFile "error.txt" AppendMode (\h -> hPutStrLn h $ show err)
    where fi = fromIntegral

--------------------------------------------------------------------------------

runDemo :: Env -> State -> IO ()
runDemo env state = void $ evalRWST run env state

run :: Demo ()
run = do
    currTime <- liftIO SDL.getTicks
    adjustWindow

    win <- asks envWindow
    state <- get

    draw
    liftIO $ do
        SDL.glSwapWindow win
        GL.flush  -- not necessary, but someone recommended it
    processEvents

    --let grid         = stateGrid state
    --    pairs        = assocs grid
    --    neighborFunc = N.getNeighbors grid
    --    sortFunc     = sortBy (\a b -> (bPos a) `compare` (bPos b))
    --    updateFunc   = (\(i, b) -> (i, updateBoid b $ take 7 $ sortFunc $ neighborFunc i))

    --modify $ \s -> s {
    --    stateGrid = grid // map updateFunc pairs
    --    }

    --let tree         = stateKDtree state
    --    boids        = KD.toList tree
    --    neighborFunc = (\b -> KD.kNearestNeighbors tree (bPos b) 7)
    --    updateFunc   = (\b -> updateBoid b $ neighborFunc b)

    --modify $ \s -> s {
    --    stateKDtree = KD.fromList $ map updateFunc boids
    --    }

    let tree         = stateOctree state
        --neighborFunc = (\b -> sortByDistance (bPos b) $ O.getRadiusObjects tree (bPos b) 3.0)
        --updateFunc   = (\b -> updateBoid b $ neighborFunc b)
        neighborFunc = (\b -> O.kNearestNeighbors tree (bPos b) 7 2.5)
        updateFunc   = (\b -> updateBoidRadius b $ neighborFunc b)

    modify $ \s -> s {
        stateOctree = O.splitWith (O.octreeMap updateFunc tree) ((> 8) . O.count)
        }

    --liftIO $ withFile "log.txt" AppendMode (\h -> hPutStrLn h $ O.prettyPrint $ stateOctree state)

    when (stateDragging state) $ do
        let sodx  = stateDragStartX      state
            sody  = stateDragStartY      state
            sodxa = stateDragStartXAngle state
            sodya = stateDragStartYAngle state
        (x, y) <- liftIO getMousePos
        let myrot = div (x - sodx) 2
            mxrot = div (y - sody) 2
        modify $ \s -> s
          { stateXAngle = sodxa + (fromIntegral mxrot)
          , stateYAngle = sodya + (fromIntegral myrot)
          }

    newTime <- liftIO SDL.getTicks
    modify $ \s -> s
      { stateGameTime = currTime
      , stateDt       = newTime - stateGameTime state
      }

    remainingTime <- remainingFrameTime
    liftIO $ SDL.delay remainingTime
    when (stateRunning state) run

remainingFrameTime :: Demo Word32
remainingFrameTime = do
    fps <- asks envFPS
    dt  <- gets stateDt
    let ticks = div 1000 (fromIntegral fps) :: Word32
    if  dt > ticks then return 0
                   else return $ ticks - dt

processEvents :: Demo ()
processEvents = do
    evPtr <- liftIO malloc :: Demo (Ptr SDL.Event)
    val   <- liftIO $ SDL.pollEvent evPtr
    case val of
      0 -> liftIO $ free evPtr
      _ -> do
          ev <- liftIO $ peek evPtr :: Demo SDL.Event
          liftIO $ free evPtr
          processEvent ev
          processEvents

processEvent :: SDL.Event -> Demo ()
processEvent ev = do
    --liftIO $ withFile "log.txt" AppendMode (\h -> hPutStrLn h $ show ev)
    case ev of
      (SDL.WindowEvent _ _ _ wev w h) -> do
          when (wev == SDL.windowEventResized) $
            modify $ \s -> s
              { stateWindowWidth  = fi w
              , stateWindowHeight = fi h
              }
          adjustWindow

      (SDL.MouseButtonEvent _ _ _ _ b st _ x y) -> do
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
                let zDist' = stateZDist s + ((realToFrac y :: Double) / (-2.0))
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

      (SDL.QuitEvent _ _) ->
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
        dispList = envBoidDispList env
    liftIO $ do
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        GL.preservingMatrix $ do
            GL.rotate (realToFrac xa) xunit
            GL.rotate (realToFrac ya) yunit
            GL.rotate (realToFrac za) zunit
            O.octreeMapM_ (drawBoid $ dispList) $ stateOctree state
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
        zfar  = 80          :: Double
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
