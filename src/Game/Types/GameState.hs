{-# LANGUAGE DuplicateRecordFields #-}

module Game.Types.GameState where

import qualified SDL
import           Linear (V2(..))
import           KeyState (KeyState, initKeyState)
import           GHC.Int (Int32)
import           Data.Map (Map, empty, fromList)

-- Video Config
data VideoConfig = VideoConfig
  { _window    :: SDL.Window
  , _glContext :: SDL.GLContext }

-- Scene
data Scene =
    Scene'Init
  | Scene'Title
  | Scene'Play
  | Scene'Quit
  deriving Eq

data SceneControl = SceneControl
  { _scene     :: Scene
  , _nextScene :: Scene }

instance Monoid SceneControl where
  mempty = SceneControl { _scene     = Scene'Init
                        , _nextScene = Scene'Init }
  mappend _ sc2 = sc2

-- Clock
-- accumulator for fixed time step
data PhysicsTime = PhysicsTime
  { _time  :: Double
  , _accum :: Double }
  deriving Show
-- global timer
data GlobalTime = GlobalTime
  { _pastTime    :: Double
  , _currentTime :: Double }
  deriving Show

data Clock = Clock
  { _globalTime  :: GlobalTime
  , _physicsTime :: PhysicsTime }

instance Monoid Clock where
  mempty = Clock { _globalTime  = GlobalTime 0 0
                 , _physicsTime = PhysicsTime { _time = 0, _accum = 0 } }


-- Input
type PlayerInputMap = (Map SDL.Keycode (KeyState Double))
type NewlyModifiedInputs = (Map SDL.Keycode Bool)
data PlayerInput = PlayerInput
  { _inputs       :: PlayerInputMap
  , _justModified :: NewlyModifiedInputs }
  deriving Show
data MousePosition = MousePosition (V2 Int32)

instance Monoid Inputs where
  mempty = Inputs { _keyboardInput = PlayerInput { _inputs       = keycodes
                                                 , _justModified = empty }
                  , _mousePosition = MousePosition $ V2 0 0 }

data Inputs = Inputs
  { _keyboardInput :: PlayerInput
  , _mousePosition :: MousePosition }

allKeys :: [SDL.Keycode]
allKeys = [ SDL.KeycodeA
          , SDL.KeycodeD
          , SDL.KeycodeW
          , SDL.KeycodeS
          , SDL.KeycodeN
          , SDL.KeycodeM
          , SDL.KeycodeP
          , SDL.KeycodeReturn
          , SDL.KeycodeRight
          , SDL.KeycodeLeft
          , SDL.KeycodeUp
          , SDL.KeycodeDown
          , SDL.KeycodeEscape
          ]

keycodes :: PlayerInputMap
keycodes = fromList $ map (\k -> (k, initKeyState)) allKeys
