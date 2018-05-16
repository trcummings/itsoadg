{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, DataKinds, ScopedTypeVariables, TypeApplications, TypeFamilies, MultiParamTypeClasses, TemplateHaskell #-}

module Main where

import Control.Monad

import Foreign.C.Types
import Linear
import Apecs

import SDL.Vect
import SDL (($=))
import qualified SDL

import Paths_itsoadg (getDataFileName)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

initialSize = V2 screenWidth screenHeight

newtype Position = Position (V2 CInt) deriving Show
instance Component Position where
  type Storage Position = Map Position

newtype Velocity = Velocity (V2 CInt) deriving Show
instance Component Velocity where
  type Storage Velocity = Map Velocity

data Player = Player deriving Show
instance Component Player where
  type Storage Player = Unique Player

data Texture = Texture SDL.Texture (V2 CInt)
instance Component Texture where
  type Storage Texture = Map Texture

makeWorld "World" [''Position, ''Velocity, ''Player, ''Texture]

type System' a = System World a

playerSpeed = 1
playerPos = V2 320 240
spriteSize = V2 32 32


loadTexture :: SDL.Renderer -> FilePath -> IO Texture
loadTexture r filePath = do
  surface <- getDataFileName filePath >>= SDL.loadBMP
  size <- SDL.surfaceDimensions surface
  let key = V4 0 maxBound maxBound maxBound
  SDL.surfaceColorKey surface $= Just key
  t <- SDL.createTextureFromSurface r surface
  SDL.freeSurface surface
  return (Texture t size)

renderTexture :: SDL.Renderer -> Texture -> Point V2 CInt -> Maybe (SDL.Rectangle CInt) -> IO ()
renderTexture r (Texture t size) xy clip =
  let dstSize = maybe size (\(SDL.Rectangle _ size') ->  size') clip
  in SDL.copy r t clip (Just (SDL.Rectangle xy dstSize))


initSystems :: Texture -> System' ()
initSystems t = void $
  newEntity (
     Player
   , Position playerPos
   , Velocity $ V2 0 0
   , t)

bumpX dirF = cmap $ \(Player, Velocity (V2 x _)) ->
  Velocity (V2 (x `dirF` playerSpeed) 0)
bumpY dirF = cmap $ \(Player, Velocity (V2 _ y)) ->
  Velocity (V2 0 (y `dirF` playerSpeed))

-- x axis
handleArrowEvent SDL.KeycodeA SDL.Pressed  = bumpX (-)
handleArrowEvent SDL.KeycodeA SDL.Released = bumpX (+)
handleArrowEvent SDL.KeycodeD SDL.Pressed  = bumpX (+)
handleArrowEvent SDL.KeycodeD SDL.Released = bumpX (-)
-- y axis
handleArrowEvent SDL.KeycodeW SDL.Pressed  = bumpY (-)
handleArrowEvent SDL.KeycodeW SDL.Released = bumpY (+)
handleArrowEvent SDL.KeycodeS SDL.Pressed  = bumpY (+)
handleArrowEvent SDL.KeycodeS SDL.Released = bumpY (-)
handleArrowEvent _ _ = return ()

handleEvent :: SDL.Event -> System' ()
handleEvent event =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
      handleArrowEvent keyCode motion
      where
        keyCode = SDL.keysymKeycode $ SDL.keyboardEventKeysym keyboardEvent
        motion  = SDL.keyboardEventKeyMotion keyboardEvent
    _ -> return ()

step :: [SDL.Event] -> SDL.Renderer -> System' ()
step events renderer = do
  mapM handleEvent events
  cmap $ \(Position p, Velocity v) -> Position (p + v)
  cmapM_ $ \(Player, Position p, Velocity v, Texture t s) -> do
    liftIO $ SDL.rendererDrawColor renderer $= V4 0 0 0 0
    liftIO $ SDL.clear renderer
    liftIO $ renderTexture
      renderer
      (Texture t s)
      (P p)
      (Just $ SDL.Rectangle (P (V2 0 0)) spriteSize)
    liftIO $ SDL.present renderer
  runGC

appLoop :: SDL.Renderer -> World -> IO ()
appLoop renderer world = do
  events <- SDL.pollEvents
  runSystem (step events renderer) world
  appLoop renderer world

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "In the Shadow of a Dead God" SDL.defaultWindow { SDL.windowInitialSize = initialSize }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  spriteSheetTexture <- loadTexture renderer "assets/red_square.bmp"

  world <- initWorld
  runSystem (initSystems spriteSheetTexture) world
  appLoop renderer world
