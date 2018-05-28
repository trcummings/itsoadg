{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Constants where

import           Linear (V2(..))
import           Foreign.C.Types (CInt)
import qualified SDL

newtype Unit =
  Unit Double
  deriving (Eq, Ord, Show, Num, Fractional)

pixelsPerUnit :: Double
pixelsPerUnit = 32

toPixels :: Unit -> CInt
toPixels (Unit a) = round (pixelsPerUnit * a)

screenWidth, screenHeight :: Unit
(screenWidth, screenHeight) = (40, 20)

initialSize :: V2 CInt
initialSize = V2 (toPixels screenWidth) (toPixels screenHeight)

maxSpeed :: Unit
maxSpeed = 17

playerPos :: V2 Unit
playerPos = V2 10 0

spriteSize :: V2 Unit
spriteSize = V2 1 1

fps :: Double
fps = 60

dT :: Double
dT = 1000 / fps

frameDeltaSeconds :: Double
frameDeltaSeconds = 1 / fps

floorFriction :: Double
floorFriction = 0.95

-- better jump

jumpPeak :: Unit
jumpPeak = 4.5

timeToJumpPeak :: Double
timeToJumpPeak = 0.5 -- in seconds

initialJumpVy :: Unit
initialJumpVy = (2 * jumpPeak) / Unit timeToJumpPeak

initialJumpG :: Unit
initialJumpG = (2 * jumpPeak) / (Unit timeToJumpPeak ^ 2)


-- horizontal movement
playerTopSpeed :: Unit
playerTopSpeed = 5

timeToTopSpeed :: Double
timeToTopSpeed = 1 -- in seconds

timeToStopFromTopSpeed :: Double
timeToStopFromTopSpeed = 0.25

runningAccel :: Unit
runningAccel = playerTopSpeed / Unit timeToTopSpeed

stoppingAccel :: Unit
stoppingAccel = (-playerTopSpeed) / Unit timeToStopFromTopSpeed
