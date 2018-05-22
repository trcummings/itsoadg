module Game.Constants where

import Linear (V2(..))
import Foreign.C.Types (CInt)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

initialSize = V2 screenWidth screenHeight

playerSpeed :: Double
playerSpeed = 75

gravity :: Double
gravity = 100

maxSpeed :: Double
maxSpeed = 100

playerPos :: V2 CInt
playerPos = V2 320 0

spriteSize :: V2 CInt
spriteSize = V2 32 32

fps :: Double
fps = 60

dT :: Double
dT = 1000 / fps
