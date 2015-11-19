module EventManager where

import Control.Monad.IO.Class    (MonadIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad             (foldM, liftM, when)
import Graphics.GPipe            (ContextT)

import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Data.Map.Strict             as M

class (Monad m) => InputStore m where
    getStateMap :: m InputStateMap
    setStateMap :: InputStateMap -> m ()

data GLFWInput = KeyInput   GLFW.Key
               | MouseInput GLFW.MouseButton
               deriving (Show, Eq, Ord)

data InputState = KeyState   GLFW.KeyState
                | MouseState GLFW.MouseButtonState
                deriving (Show, Eq, Ord)

type InputStateMap = M.Map GLFWInput InputState

data EventAction m = InputAction GLFWInput InputState (m ())
                   | CursorAction ((Double, Double) -> m ())

makeMouseEvent :: InputStore m => GLFW.MouseButton -> GLFW.MouseButtonState -> m () -> EventAction m
makeMouseEvent button state action = InputAction (MouseInput button) (MouseState state) action

makeKeyEvent :: InputStore m => GLFW.Key -> GLFW.KeyState -> m () -> EventAction m
makeKeyEvent key state action = InputAction (KeyInput key) (KeyState state) action

defaultState :: GLFWInput -> InputState
defaultState (KeyInput   _) = KeyState   GLFW.KeyState'Released
defaultState (MouseInput _) = MouseState GLFW.MouseButtonState'Released

changedState :: GLFWInput -> InputState -> InputStateMap -> (Bool, InputStateMap)
changedState input state imap =
    case M.lookup input imap of
        Just member -> (member /= state, M.insert input state imap)
        Nothing     -> (False,           M.insert input (defaultState input) imap)

currentInputState :: MonadIO m => GLFWInput -> ContextT GLFW.GLFWWindow os f m InputState
currentInputState (KeyInput   key)    = liftM KeyState   (GLFW.getKey key)
currentInputState (MouseInput button) = liftM MouseState (GLFW.getMouseButton button)

processEvents :: (MonadTrans t, MonadIO m, InputStore (t (ContextT GLFW.GLFWWindow os f m)))
              => [EventAction (t (ContextT GLFW.GLFWWindow os f m))]
              -> t (ContextT GLFW.GLFWWindow os f m) ()
processEvents events = do
    stateMap  <- getStateMap
    stateMap' <- foldM processEvent stateMap events
    setStateMap stateMap'

processEvent :: (MonadTrans t, MonadIO m, Monad (t (ContextT GLFW.GLFWWindow os f m)))
             => InputStateMap
             -> EventAction (t (ContextT GLFW.GLFWWindow os f m))
             -> t (ContextT GLFW.GLFWWindow os f m) InputStateMap
processEvent imap (InputAction input state action) = do
    currentState <- lift $ currentInputState input
    let (stateChanged, imap') = changedState input currentState imap
    when (stateChanged && (currentState == state)) action
    return imap'
processEvent imap (CursorAction action) = do
    coords <- lift $ GLFW.getCursorPos
    action coords
    return imap
