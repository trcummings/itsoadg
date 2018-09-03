module Game.Types.GameState where

import qualified SDL
import           Linear (V2(..))
import           KeyState (KeyState, initKeyState)
import           GHC.Int (Int32)
import           Data.Map (Map, empty, fromList)

-- Scene
data Scene =
    Scene'Init
  | Scene'Title
  | Scene'Play
  | Scene'Quit
  deriving (Eq, Show)

-- Clock
-- accumulator for fixed time step
data PhysicsTime =
  PhysicsTime { time :: Double, accum :: Double }
  deriving Show
-- global timer
newtype GlobalTime = GlobalTime Double deriving Show

-- Input
type PlayerInputMap = (Map SDL.Keycode (KeyState Double))
type NewlyModifiedInputs = (Map SDL.Keycode Bool)
data PlayerInput = PlayerInput
  { inputs       :: PlayerInputMap
  , justModified :: NewlyModifiedInputs }
  deriving Show
data MousePosition = MousePosition (V2 Int32)

allKeys :: [SDL.Keycode]
allKeys = [ SDL.KeycodeA
          , SDL.KeycodeD
          , SDL.KeycodeW
          , SDL.KeycodeS
          , SDL.KeycodeN
          , SDL.KeycodeM
          , SDL.KeycodeReturn
          , SDL.KeycodeRight
          , SDL.KeycodeLeft
          , SDL.KeycodeUp
          , SDL.KeycodeDown
          , SDL.KeycodeEscape
          ]

keycodes :: PlayerInputMap
keycodes = fromList $ map (\k -> (k, initKeyState)) allKeys

-- Game State
data GameState = GameState
  { _scene         :: Scene
  , _nextScene     :: Scene
  , _globalClock   :: GlobalTime
  , _physicsClock  :: PhysicsTime
  , _playerInput   :: PlayerInput
  , _mousePosition :: MousePosition }

initGameState :: GameState
initGameState =
  GameState { _scene         = Scene'Init
            , _nextScene     = Scene'Title
            , _globalClock   = GlobalTime 0
            , _physicsClock  = PhysicsTime { time = 0, accum = 0 }
            , _playerInput   = PlayerInput { inputs       = keycodes
                                           , justModified = empty }
            , _mousePosition = MousePosition $ V2 0 0 }
