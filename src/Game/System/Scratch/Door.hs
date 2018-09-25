module Game.System.Scratch.Door where

import Game.Util.Move       (defaultMoveable)
import Game.World.TH        (ECS, World)
import Game.World.Hierarchy (HierarchyCons(..), hNewEntity)
import Game.Types
  ( Position3D(..)
  , Orientation(..)
  , Hierarchy(..)
  , Door(..)
  , DoorPanel(..)
  )

initDoor :: ECS ()
initDoor = do
  let topPanel = (DoorPanel'Top,    defaultMoveable)
      botPanel = (DoorPanel'Bottom, defaultMoveable)
      door     = (Door, defaultMoveable)

  hNewEntity
    Nothing
    (HierarchyCons
      (Door, defaultMoveable)
      [ HierarchyCons topPanel []
      , HierarchyCons botPanel [] ] )
  return ()
