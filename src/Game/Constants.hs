{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Constants where

import Linear (V2(..))
import Foreign.C.Types (CInt)

newtype Unit =
  Unit Double
  deriving (Eq, Ord, Show, Num, Fractional)

pixelsPerUnit :: Double
pixelsPerUnit = 32

toPixels :: Unit -> CInt
toPixels (Unit a) = round (pixelsPerUnit * a)

screenWidth, screenHeight :: Unit
(screenWidth, screenHeight) = (20, 15)

initialSize :: V2 CInt
initialSize = V2 (toPixels screenWidth) (toPixels screenHeight)

playerSpeed :: Unit
playerSpeed = 20

gravity :: Unit
gravity = 10

maxSpeed :: Unit
maxSpeed = 30

playerPos :: V2 Unit
playerPos = V2 10 0

spriteSize :: V2 Unit
spriteSize = V2 1 1

fps :: Double
fps = 60

dT :: Double
dT = 1000 / fps

dTinSeconds :: Double
dTinSeconds = 1 / fps

floorFriction :: Double
floorFriction = 0.90
