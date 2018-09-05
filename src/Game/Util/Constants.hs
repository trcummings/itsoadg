{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Util.Constants where

import qualified SDL
import           Linear (V2(..))
import           Foreign.C.Types (CInt)
import           System.FilePath ((</>))

import           Game.Types (Unit(..), Seconds(..))

-- Window/World constants
pixelsPerUnit :: Double
pixelsPerUnit = 32

onePixel :: Unit
onePixel = Unit $ 1 / pixelsPerUnit

toPixels :: Unit -> CInt
toPixels (Unit a) = round (pixelsPerUnit * a)

screenWidth, screenHeight :: Unit
(screenWidth, screenHeight) = (40, 20)

initialSize :: V2 CInt
initialSize = V2 (toPixels screenWidth) (toPixels screenHeight)

-- Loader constants

shaderPath :: FilePath
shaderPath = "assets" </> "glsl"

texturePath :: FilePath
texturePath = "assets" </> "sprites"

-- Physics constants

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

initialFallG :: Unit
initialFallG = initialJumpG * (Unit 3)


-- horizontal movement
makeStoppingAccel :: Unit -> Unit
makeStoppingAccel topSpeed = (-topSpeed) / Unit timeToStopFromTopSpeed

makeRunningAccel :: Unit -> Unit
makeRunningAccel topSpeed = topSpeed / Unit timeToTopSpeed

playerTopSpeed :: Unit
playerTopSpeed = 10

timeToTopSpeed :: Double
timeToTopSpeed = 1 -- in seconds

timeToStopFromTopSpeed :: Double
timeToStopFromTopSpeed = 0.25

runningAccel :: Unit
runningAccel = makeRunningAccel playerTopSpeed

stoppingAccel :: Unit
stoppingAccel = makeStoppingAccel playerTopSpeed
