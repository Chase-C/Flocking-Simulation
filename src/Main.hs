{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, Rank2Types, FlexibleInstances #-}

module Main (main) where

--------------------------------------------------------------------------------

import System.IO
import Control.Parallel.Strategies    (parMap, rdeepseq)
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
import qualified OctreePar   as O

--------------------------------------------------------------------------------

data Env os c ds = Env
    { envFPS           :: !Int
    , envZDistClosest  :: !Float
    , envZDistFarthest :: !Float
    , envMVP           :: Buffer os (Uniform (V4 (B4 Float)))
    , envBoidPositions :: Buffer os (B3 Float, B3 Float)
    , envBoidVerts     :: Buffer os (B3 Float)
    , envShader        :: PrimitiveArray Triangles (B3 Float, (B3 Float, B3 Float)) -> Render os (ContextFormat c ds) ()
    }

data State = State
    { stateRunning   :: !Bool
    , stateInputMap  :: InputStateMap
    , stateXAngle    :: !Float
    , stateYAngle    :: !Float
    , stateZDist     :: !Float
    , stateZoomDir   :: !Float
    , stateMouseDown :: !Bool
    , stateDragging  :: !Bool
    , stateDragX     :: !Float
    , stateDragY     :: !Float
    , stateDragOldX  :: !Float
    , stateDragOldY  :: !Float
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
          , makeKeyEvent   GLFW.Key'W         GLFW.KeyState'Pressed          (zoomOnAction True)
          , makeKeyEvent   GLFW.Key'W         GLFW.KeyState'Released         zoomOffAction
          , makeKeyEvent   GLFW.Key'S         GLFW.KeyState'Pressed          (zoomOnAction False)
          , makeKeyEvent   GLFW.Key'S         GLFW.KeyState'Released         zoomOffAction
          , makeKeyEvent   GLFW.Key'Q         GLFW.KeyState'Pressed          closeAction
          , makeKeyEvent   GLFW.Key'Escape    GLFW.KeyState'Pressed          closeAction
          ]

main :: IO ()
main = do
    let width    = 1600
        height   = 1000
        numBoids = 1000
        bounds   = 28
        winConf :: GLFW.WindowConf
        winConf = GLFW.WindowConf width height "Flocking Simulation"

    runContextT (GLFW.newContext' [] winConf) (ContextFormatColorDepth RGB8 Depth16) $ do
        uMVP      :: Buffer os (Uniform (V4 (B4 Float))) <- newBuffer 1
        boidPos   :: Buffer os (B3 Float, B3 Float) <- newBuffer numBoids
        boidVerts :: Buffer os (B3 Float)           <- newBuffer 14
        writeBuffer boidVerts 0 [ V3   0.0    0.1  (-0.1)
                                , V3   0.0    0.0    0.4
                                , V3 (-0.2)   0.0  (-0.1)
                                , V3   0.0  (-0.1) (-0.1)
                                , V3   0.0    0.1  (-0.1)
                                , V3   0.2    0.0  (-0.1)
                                , V3   0.0    0.0    0.4
                                , V3   0.0  (-0.1) (-0.1)
                                ]

        boids <- liftIO $ makeBoids (-bounds, -bounds, -bounds)
                                    ( bounds,  bounds,  bounds) numBoids

        shader <- compileShader $ do
            primitiveStream <- toPrimitiveStream id
            mvp             <- getUniform (const (uMVP, 0))
            let primitiveStream' = fmap (transformStream mvp) primitiveStream
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
              , stateZoomDir   = 0
              , stateMouseDown = False
              , stateDragging  = False
              , stateDragX     = 0
              , stateDragY     = 0
              , stateDragOldX  = 0
              , stateDragOldY  = 0
              , stateOctree    = O.splitWith (O.fromList boids (V3 0 0 0) 32) ((> 8) . O.count)
              }

        liftIO $ swapInterval 1
        runSim env state

transformStream :: Floating a => M44 a -> (V3 a, (V3 a, V3 a)) -> (V4 a, V3 a)
transformStream mvp (V3 x y z, (pos, dir)) = (transformMat !* (V4 x y z 1), V3 0.7 0.2 0.4)
    where
        normDir      = vNorm dir
        axis         = cross (V3 0 0 1) normDir
        angle        = acos $ vDot normDir (V3 0 0 1)
        sinA         = sin $ angle / 2
        cosA         = cos $ angle / 2
        qAxis        = sinA *^ axis
        quat         = Quaternion cosA qAxis
        rotationMat  = mkTransformation quat pos
        transformMat = mvp !*! rotationMat

--------------------------------------------------------------------------------

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
    handleCamera

    env         <- ask
    tree        <- gets stateOctree
    viewProjMat <- makeMVP

    let (bs, tree')  = updateOctree tree
        positions    = map (\b -> (bPos b, bVel b)) bs
        uMVP         = envMVP           env
        posB         = envBoidPositions env
        vertB        = envBoidVerts     env
        shader       = envShader        env

    modify $ \s -> s
        { stateOctree = tree'
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
    running        <- gets stateRunning
    unless (closeRequested || not running) run

--------------------------------------------------------------------------------

handleCamera :: Sim os c ds ()
handleCamera = do
    state <- get
    env   <- ask
    let zmin       = envZDistClosest  env
        zmax       = envZDistFarthest env
        zdir       = stateZoomDir     state
        z          = stateZDist       state
        x          = stateDragX       state
        y          = stateDragY       state
        x'         = stateDragOldX    state
        y'         = stateDragOldY    state
        xa         = stateXAngle      state
        ya         = stateYAngle      state
        (xa', ya') = if stateDragging state
                       then (xa + ((x - x') / 512), ya + ((y - y') / 512))
                       else (xa, ya)
    put $ state
        { stateXAngle = xa'
        , stateYAngle = ya'
        , stateZDist  = min zmax (max zmin (z + (zdir)))
        }

makeMVP :: Sim os c ds (M44 Float)
makeMVP = do
    state    <- get
    (V2 w h) <- lift getContextBuffersSize
    let xa       = stateXAngle state
        ya       = stateYAngle state
        zDist    = stateZDist  state
        xQuat    = axisAngle (V3 0 1 0) xa
        yQuat    = axisAngle (V3 1 0 0) ya
        modelRot = fromQuaternion $ yQuat * xQuat
        modelMat = mkTransformationMat modelRot (V3 0 0 0)
        projMat  = perspective (pi/3) (fromIntegral w / fromIntegral h) 1 100
        viewMat  = mkTransformationMat identity (- V3 0 0 zDist)
    return $ projMat !*! viewMat !*! modelMat

updateOctree :: O.Octree -> ([Boid], O.Octree)
updateOctree tree = (boids', tree')
    where center       = O.center      tree
          len          = O.len         tree
          boids        = O.flattenTree tree
          neighborFunc = (\b -> O.kNearestNeighbors tree (bPos b) 7 (bRad b))
          updateFunc   = (\b -> updateBoidRadius b $ neighborFunc b)
          boids'       = parMap rdeepseq updateFunc boids
          --boids'       = map updateFunc boids
          tree'        = O.splitWith (O.fromList boids' center len) ((> 8) . O.count)

--------------------------------------------------------------------------------

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

zoomOnAction :: Bool -> Sim os c ds ()
zoomOnAction zoomIn = modify $ \s -> s { stateZoomDir = if zoomIn then (-0.3) else 0.3 }

zoomOffAction :: Sim os c ds ()
zoomOffAction = modify $ \s -> s { stateZoomDir = 0 }
