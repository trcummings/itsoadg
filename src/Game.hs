{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, DataKinds, ScopedTypeVariables, TypeApplications, TypeFamilies, MultiParamTypeClasses, TemplateHaskell #-}

module Game (main) where

import Control.Monad

import Data.Maybe (catMaybes)
import Foreign.C.Types
import Linear
import Apecs

import Data.Text (singleton)

import SDL.Vect
import SDL.Time (ticks)
import qualified SDL.Font as TTF
import SDL (($=))
import qualified SDL

import Paths_itsoadg (getDataFileName)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

initialSize = V2 screenWidth screenHeight

newtype Position = Position (V2 CInt) deriving Show
instance Component Position where
  type Storage Position = Map Position

newtype Velocity = Velocity (V2 Double) deriving Show
instance Component Velocity where
  type Storage Velocity = Map Velocity

newtype Acceleration = Acceleration (V2 Double) deriving Show
instance Component Acceleration where
  type Storage Acceleration = Map Acceleration

newtype BoundingBox = BoundingBox (V2 CInt) deriving Show -- h x w
instance Component BoundingBox where
  type Storage BoundingBox = Map BoundingBox

data Player = Player deriving Show
instance Component Player where
  type Storage Player = Unique Player

data Camera = Camera deriving Show
instance Component Camera where
  type Storage Camera = Unique Camera

data Floor = Floor deriving Show
instance Component Floor where
  type Storage Floor = Map Floor

data Texture = Texture SDL.Texture (V2 CInt)
instance Component Texture where
  type Storage Texture = Map Texture

data Gravity = Gravity
instance Component Gravity where
  type Storage Gravity = Map Gravity

newtype Friction = Friction Double
instance Component Friction where
  type Storage Friction = Map Friction

data Font = Font [(Char, Texture)]
instance Component Font where
  type Storage Font = Map Font

data Collisions = Collisions [Entity] deriving Show
instance Component Collisions where
  type Storage Collisions = Map Collisions

-- accumulator for physics frame time updates
data PhysicsTime = PhysicsTime
  { time  :: Double
  , accum :: Double }
  deriving Show

instance Monoid PhysicsTime where
  mempty = PhysicsTime 0 0

instance Component PhysicsTime where
  type Storage PhysicsTime = Global PhysicsTime

-- global timer
newtype GlobalTime = GlobalTime Double deriving Show

instance Monoid GlobalTime where
  mempty = GlobalTime 0

instance Component GlobalTime where
  type Storage GlobalTime = Global GlobalTime

makeWorld "World" [
    ''Position
  , ''Velocity
  , ''Acceleration
  , ''BoundingBox
  , ''Friction
  , ''Player
  , ''Collisions
  , ''Floor
  , ''Texture
  , ''GlobalTime
  , ''PhysicsTime
  , ''Gravity
  , ''Camera
  , ''Font
  ]

type System' a = System World a

playerSpeed = 75
gravity = 100
maxSpeed = 100
playerPos = V2 320 0
spriteSize = V2 32 32

fps :: Double
fps = 60

dT :: Double
dT = 1000 / fps

toTexture :: SDL.Renderer -> SDL.Surface -> IO Texture
toTexture r surface = do
  size <- SDL.surfaceDimensions surface
  let key = V4 0 maxBound maxBound maxBound
  SDL.surfaceColorKey surface $= Just key
  t <- SDL.createTextureFromSurface r surface
  -- once we've made a texture from the surface, free the surface
  SDL.freeSurface surface
  return (Texture t size)

loadTexture :: SDL.Renderer -> FilePath -> IO Texture
loadTexture r filePath = do
  surface <- getDataFileName filePath >>= SDL.loadBMP
  toTexture r surface

renderTexture :: SDL.Renderer
              -> Texture
              -> Point V2 CInt
              -> Maybe (SDL.Rectangle CInt)
              -> IO ()
renderTexture r (Texture t size) xy clip =
  let dstSize = maybe size (\(SDL.Rectangle _ size') ->  size') clip
  in SDL.copy r t clip (Just (SDL.Rectangle xy dstSize))

initSystems :: SDL.Renderer -> System' ()
initSystems renderer = void $ do
  -- load in assets, convert to textures
  spriteSheetTexture <- liftIO $ loadTexture renderer "assets/red_square.bmp"
  smallFont <- liftIO $ TTF.load "assets/04B_19__.TTF" 24
  let characters = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ', ':', ',', '-', '.']
  fontMap <- liftIO $ mapM (\c -> do
        texture <- toTexture renderer =<< TTF.blended
          smallFont
          (V4 255 255 255 255)
          (singleton c)
        return (c, texture)
      ) $ characters
  -- after we convert our font to textures we dont need the resource anymore
  TTF.free smallFont

  -- entities
  newEntity ( -- player
      Player
    , Position playerPos
    , Velocity $ V2 0 0
    , Acceleration $ V2 0 0
    , BoundingBox spriteSize
    , Gravity
    , Collisions []
    , spriteSheetTexture )

  newEntity ( -- small font
      Position $ V2 0 0
    , Font fontMap )

  newEntity ( -- floor
      Floor
    , Position $ V2 0 (screenHeight - 20)
    , Friction 0.3
    , Collisions []
    , BoundingBox (V2 screenWidth 20) )

-- constant acceleration
bumpX x = cmap $ \(Player, Acceleration (V2 _ y)) -> Acceleration $ V2 x y

-- x axis
handleArrowEvent SDL.KeycodeA SDL.Pressed  = bumpX (-playerSpeed)
handleArrowEvent SDL.KeycodeA SDL.Released = bumpX 0
handleArrowEvent SDL.KeycodeD SDL.Pressed  = bumpX playerSpeed
handleArrowEvent SDL.KeycodeD SDL.Released = bumpX 0
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

toAABB :: V2 CInt -> V2 CInt -> V4 CInt
toAABB (V2 x y) (V2 w h) = V4 x y (x + w) (y + h)

aabbIntersection :: V4 CInt -> V4 CInt -> Bool
aabbIntersection (V4 tlx1 tly1 brx1 bry1) (V4 tlx2 tly2 brx2 bry2) =
  (brx1 >= tlx2) && (tlx1 <= brx2) && (bry1 >= tly2) && (tly1 <= bry2)

handleFloor :: Entity -> System' ()
handleFloor e = do
  (_, Friction f, Position p') <- get e :: System' (Floor, Friction, Position)
  cmap $ \(Player, Velocity (V2 vx _)) -> Velocity $ V2 vx 0
  cmap $ \(Player, Acceleration (V2 ax ay)) -> Acceleration $ V2 (ax * f) ay

clampVelocity :: Double -> Double
clampVelocity v =
  if (v > 0)
  then min v maxSpeed
  else max v (-maxSpeed)

runPhysics :: Double -> System' ()
runPhysics dT = do
  let dTs = (dT / 1000)
    -- update acceleration based on gravity
  cmap $ \(Gravity, Acceleration (V2 x _)) -> Acceleration $ V2 x gravity

  -- detect collisions
  aabbs <- getAll :: System' [(BoundingBox, Position, Entity)]
  -- O(n^2) algorithm, optimize later
  mapM (\(BoundingBox bb, Position p, entity) -> do
      let shouldAABB = aabbIntersection $ toAABB p bb
          actives = filter (\(BoundingBox bb', Position p', e') ->
            (not $ e' == entity) && (shouldAABB $ toAABB p' bb')) aabbs
          entities = map (\(_, _, e) -> e) actives
      set entity (Collisions entities)
    ) aabbs

  -- resolve collisions
  cmapM_ $ \(Player, Collisions es) -> do
    -- loop over each collision. if its a wall, if its a floor, else ignore
    mapM (\e -> do
     isFloor <- exists e (proxy :: (Floor, Position))
     when isFloor $ (handleFloor e) ) es

  -- clear remaining collisions
  cmap $ \(Collisions _) -> Collisions []

  -- update velocity based on acceleration
  cmap $ \(Acceleration a, Velocity v) ->
    let (V2 vx vy) = (v + (a ^* dTs))
    in Velocity $ V2 (clampVelocity vx) (clampVelocity vy)

  -- update position based on time and velocity
  cmap $ \(Acceleration a, Velocity v, Position p) ->
    Position $ roundV2 $ (cIntToDouble p) + ((v ^* dTs) + (a ^* (0.5 * dTs ^ 2)))

  -- clamp player position to screen edges
  cmap $ \(Player, Position (V2 x y)) -> Position $ V2
    ((min (screenWidth  - 32) . max 0 $ x) :: CInt)
    ((min (screenHeight - 32) . max 0 $ y) :: CInt)

updatePhysicsAccum :: Double -> System' ()
updatePhysicsAccum nextTime = do
  GlobalTime currentTime <- get global
  -- update global time
  cmap $ \(GlobalTime _) -> GlobalTime nextTime

  -- update physics frame time accumulator
  cmap $ \(PhysicsTime t acc) -> PhysicsTime
    { time = t
    -- clamp frameTime at 25ms
    , accum = acc + (min 25 $ nextTime - currentTime) }

-- update physics multiple times if time step is less than frame update time
runPhysicsLoop :: System' ()
runPhysicsLoop = do
  PhysicsTime t acc <- get global
  when (acc >= dT) $ do
    runPhysics dT
    cmap $ \(PhysicsTime t' acc') -> PhysicsTime
      { time = (t' + dT)
      , accum = (acc' - dT) }
    runPhysicsLoop

roundV2 :: V2 Double -> V2 CInt
roundV2 (V2 a b) = V2 (round a) (round b)

cIntToDouble :: V2 CInt -> V2 Double
cIntToDouble (V2 a b) = V2 (fromIntegral a) (fromIntegral b)

step :: Double -> [SDL.Event] -> SDL.Window -> SDL.Renderer -> System' ()
step nextTime events window renderer = do
  -- update velocity based on arrow key presses
  mapM handleEvent events

  -- update physics
  updatePhysicsAccum nextTime
  runPhysicsLoop

  -- render "player"
  cmapM_ $ \(Player, Position p, Velocity v, Texture t s) -> do
    liftIO $ renderTexture
      renderer
      (Texture t s)
      (P p)
      (Just $ SDL.Rectangle (P (V2 0 0)) spriteSize)

  -- render small font
  cmapM_ $ \(Font f, Position p) -> do
    cmapM_ $ \(Player, Position pp, Velocity pv) -> do
      let pText = "Player: " ++ (show pp) ++ ", " ++ (show $ roundV2 pv)
          textures = catMaybes (map (\c -> lookup c f) pText)
          spacingMap = [xy | xy <- [1..700], xy `mod` 14 == 0]
          textPosMap = zip textures spacingMap
      mapM_ (\(Texture t s, pMod) -> do
        liftIO $ renderTexture
          renderer
          (Texture t s)
          (P $ p + (V2 pMod 0))
          (Just $ SDL.Rectangle (P (V2 0 0)) s)
            ) (take 30 textPosMap)

  -- render bounding boxes
  cmapM_ $ \(BoundingBox (V2 w h), Position (V2 x y)) -> do
    liftIO $ SDL.rendererDrawColor renderer $= V4 0 0 maxBound maxBound
    liftIO $ SDL.drawRect
      renderer
      (Just $ SDL.Rectangle (P (V2 x y)) (V2 w h))

  -- garbage collect. yes, every frame
  runGC

appLoop :: SDL.Window -> SDL.Renderer -> World -> IO ()
appLoop window renderer world = do
  -- prep next render
  liftIO $ SDL.rendererDrawColor renderer $= V4 0 0 0 0
  liftIO $ SDL.clear renderer

  -- get next time tick from SDL
  nextTime <- ticks

  -- collect events from SDL
  events <- SDL.pollEvents

  -- run main system
  runSystem (step (fromIntegral nextTime) events window renderer) world

  -- run current render
  liftIO $ SDL.present renderer

  -- loop
  appLoop window renderer world

main :: IO ()
main = do
  SDL.initializeAll -- initialize all SDL systems
  TTF.initialize   -- initialize SDL.Font

  -- create window and renderer
  window <- SDL.createWindow "ITSOADG" SDL.defaultWindow { SDL.windowInitialSize = initialSize }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  -- initialize Apecs world & add entities
  world <- initWorld
  runSystem (initSystems renderer) world

  -- start loop
  appLoop window renderer world
