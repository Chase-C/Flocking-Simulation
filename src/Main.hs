{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, Rank2Types, FlexibleInstances #-}

module Main (main) where

--------------------------------------------------------------------------------

import System.IO
--import Control.Concurrent.STM       (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Monad                (unless, when, void)
--import Control.Monad.Par            (Par, runPar, parMap)
import Control.Monad.Trans.RWS.Strict(RWST, ask, asks, evalRWST, get, gets, modify, put)
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Class    (lift)
--import Foreign.C.String             (CString, newCString, peekCString)
--import Foreign.C.Types              (CInt)
--import Foreign.Marshal.Alloc        (malloc, free)
--import Foreign.Ptr                  (Ptr)
--import Foreign.Storable             (peek)
--import Data.Bits                    ((.|.))
--import Data.Word                    (Word32)
--import Data.Array.IArray            ((//), assocs, elems)
--import Data.List                    (sortBy)

import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import Graphics.UI.GLFW (swapInterval)
import Linear

import Boid
import EventManager
import Utils

import qualified Data.Map as M
import qualified Octree   as O

--------------------------------------------------------------------------------

data Env os c ds = Env
    { envFPS           :: !Int
    , envZDistClosest  :: !Float
    , envZDistFarthest :: !Float
    , envMVP           :: Buffer os (Uniform (V4 (B4 Float)))
    , envBoidPositions :: Buffer os (B3 Float)
    , envBoidVerts     :: Buffer os (B3 Float)
    , envShader        :: PrimitiveArray Triangles (B3 Float, B3 Float) -> Render os (ContextFormat c ds) ()
    }

data State = State
    { stateRunning         :: !Bool
    , stateInputMap        :: InputStateMap
    , stateGameTime        :: !Double
    , stateDt              :: !Double
    , stateWindowWidth     :: !Int
    , stateWindowHeight    :: !Int
    , stateXAngle          :: !Float
    , stateYAngle          :: !Float
    , stateZAngle          :: !Float
    , stateZDist           :: !Float
    , stateMouseDown       :: !Bool
    , stateDragging        :: !Bool
    , stateDragStartX      :: !Int
    , stateDragStartY      :: !Int
    , stateDragStartXAngle :: !Float
    , stateDragStartYAngle :: !Float
    , stateBoids           :: ![Boid]
    , stateOctree          :: !O.Octree
    }

instance (Monad m, Monoid w) => InputStore (RWST r w State m) where
    getStateMap   = gets stateInputMap
    setStateMap m = modify $ \s -> s { stateInputMap = m }

type Sim os c ds = RWST (Env os c ds) () State (ContextT GLFW.GLFWWindow os (ContextFormat c ds) IO)

--mouseDownAction :: Sim os c ds ()
--mouseDownAction = modify $ \s -> s { stateMouseDown = True }
--    (SDL.MouseButtonEvent _ _ _ _ b st _ x y) -> do
--        when (b == SDL.SDL_BUTTON_LEFT) $ do
--            let pressed = (st == 1)
--            modify $ \s -> s
--            { stateMouseDown = pressed
--        }
--        unless pressed $
--        modify $ \s -> s
--        { stateDragging = False
--    }

--------------------------------------------------------------------------------


proj :: Floating a => M44 a -> (V3 a, V3 a) -> (V4 a, V3 a)
proj modelViewProj (V3 px py pz, c) = (modelViewProj !* V4 px py pz 1, c)

offsetVert :: Floating a => (V3 a, V3 a) -> (V3 a, V3 a)
offsetVert (V3 xx yy zz, V3 a b c) =  (V3 x y z, V3 0.7 0.2 0.4)
    where
        x = xx + a
        y = yy + b
        z = zz + c

main :: IO ()
main = do
    let width    = 1200
        height   = 800
        numBoids = 1000
        winConf :: GLFW.WindowConf
        winConf = GLFW.WindowConf width height "Flocking Simulation"

    runContextT (GLFW.newContext' [] winConf) (ContextFormatColorDepth RGB8 Depth16) $ do
        uMVP      :: Buffer os (Uniform (V4 (B4 Float))) <- newBuffer 1
        boidPos   :: Buffer os (B3 Float)                <- newBuffer numBoids
        boidVerts :: Buffer os (B3 Float)                <- newBuffer 14
        writeBuffer boidVerts 0 [ V3 (-0.1) (-0.1)   0.1
                                , V3   0.1  (-0.1)   0.1
                                , V3 (-0.1)   0.1    0.1
                                , V3   0.1    0.1    0.1
                                , V3   0.1    0.1  (-0.1)
                                , V3   0.1  (-0.1)   0.1
                                , V3   0.1  (-0.1) (-0.1)
                                , V3 (-0.1) (-0.1) (-0.1)
                                , V3   0.1    0.1  (-0.1)
                                , V3 (-0.1)   0.1  (-0.1)
                                , V3 (-0.1)   0.1    0.1
                                , V3 (-0.1) (-0.1) (-0.1)
                                , V3 (-0.1) (-0.1)   0.1
                                , V3   0.1  (-0.1)   0.1
                                ]

        boids <- liftIO $ makeBoids (-32, -32, -32) (32, 32, 32) numBoids

        shader <- compileShader $ do
            primitiveStream <- toPrimitiveStream id
            modelViewProj   <- getUniform (const (uMVP, 0))
            let primitiveStream' = fmap (proj modelViewProj . offsetVert) primitiveStream
                colorOption      = ContextColorOption NoBlending (V3 True True True)
                depthOption      = DepthOption Less True
                rasterOptions    = (FrontAndBack, ViewPort 0 (V2 width height), DepthRange 0 1)

            fragmentStream <- rasterize (const rasterOptions) primitiveStream'
            let getZ (V4 _ _ z _) = z
                fragmentStream'   = withRasterizedInfo
                    (\a x -> (a, getZ $ rasterizedFragCoord x)) fragmentStream

            drawContextColorDepth (const (colorOption, depthOption)) fragmentStream'

        let zDistClosest  = 10
            zDistFarthest = zDistClosest + 80
            zDist         = zDistClosest + ((zDistFarthest - zDistClosest) / 2)
            env = Env
              { envFPS           = 60
              , envZDistClosest  = zDistClosest
              , envZDistFarthest = zDistFarthest
              , envMVP           = uMVP
              , envBoidPositions = boidPos
              , envBoidVerts     = boidVerts
              , envShader        = shader
              }
            state = State
              { stateRunning         = True
              , stateInputMap        = M.empty
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
              , stateOctree          = O.splitWith (O.fromList boids (V3 0 0 0) 32) ((> 8) . O.count)
              }

        liftIO $ swapInterval 1
        runSim env state

runSim :: (ContextColorFormat c, Color c Float ~ V3 Float, DepthRenderable ds)
       => Env os c ds -> State -> ContextT GLFW.GLFWWindow os (ContextFormat c ds) IO ()
runSim env state =
    void $ evalRWST run env state

run :: (ContextColorFormat c, Color c Float ~ V3 Float, DepthRenderable ds)
    => Sim os c ds ()
run = do
    --currTime <- liftIO GLFW.getTime
    --adjustWindow

    --processEvents actions

    env   <- ask
    state <- get
    (V2 w h) <- lift getContextBuffersSize

    let octree       = stateOctree state
        cen          = O.center octree
        len          = O.len octree
        boids        = O.flattenTree octree
        neighborFunc = (\b -> O.kNearestNeighbors octree (bPos b) 7 (bRad b))
        updateFunc   = (\b -> updateBoidRadius b $ neighborFunc b)
        --newTree      = O.splitWith (O.fromList (runPar $ parMap updateFunc boids) cen len) ((> 8) . O.count)
        newTree      = O.splitWith (O.fromList (map updateFunc boids) cen len) ((> 8) . O.count)
        positions    = map bPos $ O.flattenTree newTree
        uMVP         = envMVP env
        posB         = envBoidPositions env
        vertB        = envBoidVerts env
        shader       = envShader env
        --angle        = vlog $ stateXAngle state
        angle        = stateXAngle state
        modelRot     = fromQuaternion (axisAngle (V3 1 0.5 0.3) angle)
        --modelRot     = identity
        modelMat     = mkTransformationMat modelRot (V3 0 0 0)
        projMat      = perspective (pi/3) (fromIntegral w / fromIntegral h) 1 100
        viewMat      = mkTransformationMat identity (- V3 0 0 50)
        viewProjMat  = projMat !*! viewMat !*! modelMat

    modify $ \s -> s {
        stateOctree = newTree
        }

    lift $ do
        writeBuffer posB 0 $ positions
        writeBuffer uMVP 0 [viewProjMat]
        render $ do
            clearContextColor (V3 0 0 0)
            clearContextDepth 1
            vertArray <- newVertexArray vertB
            posArray  <- newVertexArray posB
            shader $ toPrimitiveArrayInstanced TriangleStrip (,) vertArray posArray
        swapContextBuffers

    closeRequested <- lift $ GLFW.windowShouldClose
    unless closeRequested run

actions :: [EventAction (Sim os c ds)]
actions = [makeMouseEvent GLFW.MouseButton'1 GLFW.MouseButtonState'Pressed mouseDownAction]

mouseDownAction :: Sim os c ds ()
mouseDownAction = do
    angle <- gets stateXAngle
    modify $ \s -> s { stateXAngle = angle + 0.1 }


    --when (stateDragging state) $ do
    --    let sodx  = stateDragStartX      state
    --        sody  = stateDragStartY      state
    --        sodxa = stateDragStartXAngle state
    --        sodya = stateDragStartYAngle state
    --    (x, y) <- liftIO getMousePos
    --    let myrot = div (x - sodx) 2
    --        mxrot = div (y - sody) 2
    --    modify $ \s -> s
    --      { stateXAngle = sodxa + fromIntegral mxrot
    --      , stateYAngle = sodya + fromIntegral myrot
    --      }

    --newTime <- liftIO SDL.getTicks
    --modify $ \s -> s
    --  { stateGameTime = currTime
    --  , stateDt       = newTime - stateGameTime state
    --  }

    --remainingTime <- remainingFrameTime
    --liftIO $ SDL.delay remainingTime

--remainingFrameTime :: Demo Word32
--remainingFrameTime = do
--    fps <- asks envFPS
--    dt  <- gets stateDt
--    let ticks = div 1000 (fromIntegral fps) :: Word32
--    if  dt > ticks then return 0
--                   else return $ ticks - dt
--
--processEvents :: Demo ()
--processEvents = do
--    evPtr <- liftIO malloc :: Demo (Ptr SDL.Event)
--    val   <- liftIO $ SDL.pollEvent evPtr
--    case val of
--      0 -> liftIO $ free evPtr
--      _ -> do
--          ev <- liftIO $ peek evPtr :: Demo SDL.Event
--          liftIO $ free evPtr
--          processEvent ev
--          processEvents
--
--processEvent :: SDL.Event -> Demo ()
--processEvent ev = do
--    --liftIO $ withFile "log.txt" AppendMode (\h -> hPutStrLn h $ show ev)
--    case ev of
--      (SDL.WindowEvent _ _ _ wev w h) -> do
--          when (wev == SDL.SDL_WINDOWEVENT_RESIZED) $
--            modify $ \s -> s
--              { stateWindowWidth  = fi w
--              , stateWindowHeight = fi h
--              }
--          adjustWindow
--
--      (SDL.MouseButtonEvent _ _ _ _ b st _ x y) -> do
--          when (b == SDL.SDL_BUTTON_LEFT) $ do
--              let pressed = (st == 1)
--              modify $ \s -> s
--                { stateMouseDown = pressed
--                }
--              unless pressed $
--                modify $ \s -> s
--                  { stateDragging = False
--                  }
--
--      (SDL.MouseMotionEvent _ _ _ _ _ x y _ _) -> do
--          state <- get
--          when (stateMouseDown state && not (stateDragging state)) $
--            put $ state
--              { stateDragging        = True
--              , stateDragStartX      = fi x
--              , stateDragStartY      = fi y
--              , stateDragStartXAngle = stateXAngle state
--              , stateDragStartYAngle = stateYAngle state
--              }
--
--      (SDL.MouseWheelEvent _ _ _ _ _ y) -> do
--          env <- ask
--          modify $ \s -> s
--            { stateZDist =
--                let zDist' = stateZDist s + ((realToFrac y :: Float) / (-2.0))
--                in  curb (envZDistClosest env) (envZDistFarthest env) zDist'
--            }
--          adjustWindow
--
--      (SDL.KeyboardEvent t _ _ _ x (SDL.Keysym k _ _)) ->
--          when (t == SDL.SDL_KEYUP) $ do
--            -- Q, Esc: exit
--            when (k == SDL.SDL_SCANCODE_Q || k == SDL.SDL_SCANCODE_ESCAPE) $
--                modify $ \s -> s
--                  { stateRunning = False
--                  }
--
--      (SDL.QuitEvent _ _) ->
--          modify $ \s -> s
--            { stateRunning = False
--            }
--
--      _else -> return ()
--    where fi = fromIntegral
--
--draw :: Demo ()
--draw = do
--    env   <- ask
--    state <- get
--    let xa = stateXAngle state
--        ya = stateYAngle state
--        za = stateZAngle state
--        dispList = envBoidDispList env
--    liftIO $ do
--        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
--        GL.preservingMatrix $ do
--            GL.rotate (realToFrac xa) xunit
--            GL.rotate (realToFrac ya) yunit
--            GL.rotate (realToFrac za) zunit
--            O.octreeMapM_ (drawBoid $ dispList) $ stateOctree state
--      where
--        xunit = GL.Vector3 1 0 0 :: GL.Vector3 GL.GLfloat
--        yunit = GL.Vector3 0 1 0 :: GL.Vector3 GL.GLfloat
--        zunit = GL.Vector3 0 0 1 :: GL.Vector3 GL.GLfloat
--
--adjustWindow :: Demo ()
--adjustWindow = do
--    state <- get
--    let width  = stateWindowWidth  state
--        height = stateWindowHeight state
--        zDist  = stateZDist        state
--
--    let pos   = GL.Position 0 0
--        size  = GL.Size (fromIntegral width) (fromIntegral height)
--        h     = fromIntegral height / fromIntegral width :: Float
--        znear = 1           :: Float
--        zfar  = 120         :: Float
--        xmax  = znear * 0.5 :: Float
--    liftIO $ do
--        GL.viewport   GL.$= (pos, size)
--        GL.matrixMode GL.$= GL.Projection
--        GL.loadIdentity
--        GL.frustum (realToFrac $ -xmax)
--                   (realToFrac    xmax)
--                   (realToFrac $ -xmax * realToFrac h)
--                   (realToFrac $  xmax * realToFrac h)
--                   (realToFrac    znear)
--                   (realToFrac    zfar)
--        GL.matrixMode GL.$= GL.Modelview 0
--        GL.loadIdentity
--        GL.translate (GL.Vector3 0 0 (negate $ realToFrac zDist) :: GL.Vector3 GL.GLfloat)
--
----------------------------------------------------------------------------------
--
--curb :: Ord a => a -> a -> a -> a
--curb l h x
--  | x < l     = l
--  | x > h     = h
--  | otherwise = x
