{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, Rank2Types, FlexibleInstances #-}

module Main (main) where

--------------------------------------------------------------------------------

import System.IO
--import Control.Concurrent.STM       (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
--import Control.Monad.Par            (Par, runPar, parMap)
import Control.Monad                  (unless, when, void)
import Control.Monad.Trans.RWS.Strict (RWST, ask, asks, evalRWST, get, gets, modify, put)
import Control.Monad.IO.Class         (liftIO)
import Control.Monad.Trans.Class      (lift)

import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import Graphics.UI.GLFW (swapInterval)
import Linear

import Boid
import EventManager
import Utils
import Log

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
    { stateRunning   :: !Bool
    , stateInputMap  :: InputStateMap
    , stateXAngle    :: !Float
    , stateYAngle    :: !Float
    , stateZDist     :: !Float
    , stateMouseDown :: !Bool
    , stateDragging  :: !Bool
    , stateDragX     :: !Float
    , stateDragY     :: !Float
    , stateDragOldX  :: !Float
    , stateDragOldY  :: !Float
    , stateBoids     :: ![Boid]
    , stateOctree    :: !O.Octree
    }

instance (Monad m, Monoid w) => InputStore (RWST r w State m) where
    getStateMap    = gets stateInputMap
    setStateMap sm = modify $ \s -> s { stateInputMap = sm }

type Sim os c ds = RWST (Env os c ds) () State (ContextT GLFW.GLFWWindow os (ContextFormat c ds) IO)

--------------------------------------------------------------------------------

actions :: [EventAction (Sim os c ds)]
actions = [ CursorAction cursorAction
          , makeMouseEvent GLFW.MouseButton'1 GLFW.MouseButtonState'Pressed  mouseDownAction
          , makeMouseEvent GLFW.MouseButton'1 GLFW.MouseButtonState'Released mouseUpAction
          , makeKeyEvent   GLFW.Key'Q         GLFW.KeyState'Pressed          closeAction
          , makeKeyEvent   GLFW.Key'Escape    GLFW.KeyState'Pressed          closeAction
          ]

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
        numBoids = 750
        bounds   = 16
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

        boids <- liftIO $ makeBoids (-bounds, -bounds, -bounds)
                                    (bounds,   bounds,  bounds) numBoids

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
              { stateRunning   = True
              , stateInputMap  = M.empty
              , stateXAngle    = 0
              , stateYAngle    = 0
              , stateZDist     = zDist
              , stateMouseDown = False
              , stateDragging  = False
              , stateDragX     = 0
              , stateDragY     = 0
              , stateDragOldX  = 0
              , stateDragOldY  = 0
              , stateBoids     = []
              , stateOctree    = O.splitWith (O.fromList boids (V3 0 0 0) 32) ((> 8) . O.count)
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

    processEvents actions
    handleRotation

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
        xa           = stateXAngle state
        ya           = stateYAngle state
        modelXRot    = fromQuaternion (axisAngle (V3 0 1 0) xa)
        modelYRot    = fromQuaternion (axisAngle (V3 1 0 0) ya)
        modelRot     = modelYRot !*! modelXRot
        modelMat     = mkTransformationMat modelRot (V3 0 0 0)
        projMat      = perspective (pi/3) (fromIntegral w / fromIntegral h) 1 100
        viewMat      = mkTransformationMat identity (- V3 0 0 30)
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
    unless (closeRequested || not (stateRunning state)) run

handleRotation :: Sim os c ds ()
handleRotation = do
    state <- get
    when (stateDragging state) $ do
        let x  = stateDragX    state
            y  = stateDragY    state
            x' = stateDragOldX state
            y' = stateDragOldY state
            xa = stateXAngle   state
            ya = stateYAngle   state
        put $ state
            { stateXAngle = xa + ((x - x') / 512)
            , stateYAngle = ya + ((y - y') / 512)
            }

mouseDownAction :: Sim os c ds ()
mouseDownAction = modify $ \s -> s { stateMouseDown = True }

mouseUpAction :: Sim os c ds ()
mouseUpAction = modify $ \s -> s
    { stateMouseDown = False
    , stateDragging  = False
    }

cursorAction :: (Double, Double) -> Sim os c ds ()
cursorAction (x, y) = do
    state <- get
    when (stateMouseDown state) $
        let (x', y') = if stateDragging state
                         then (stateDragX state, stateDragY state)
                         else (realToFrac x,     realToFrac y)
        in put $ state
            { stateDragging = True
            , stateDragX    = realToFrac x
            , stateDragY    = realToFrac y
            , stateDragOldX = x'
            , stateDragOldY = y'
            }

closeAction :: Sim os c ds ()
closeAction = modify $ \s -> s { stateRunning = False }


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
