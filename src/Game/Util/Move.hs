module Game.Util.Move (Moveable, runMoveCommand, defaultMoveable) where

import qualified Linear as L

import Game.Types
  ( Degrees(..)
  , Rotation(..)
  , Translation(..)
  , MoveCommand(..)
  , Orientation(..)
  , Position3D(..) )

type Moveable = (Orientation, Position3D)

defaultMoveable :: Moveable
defaultMoveable = ( Orientation $ L.Quaternion 1 (L.V3 0 0 0)
                  , Position3D  $ L.V3 0 0 0 )

-- camera action commands
runMoveCommand :: MoveCommand -> (Moveable -> Moveable)
runMoveCommand (Move'Translate v    ) = translate v
runMoveCommand (Move'Rotate    r  d ) = rotate r d
runMoveCommand (Move'Compose   a1 a2) = runMoveCommand a2 .
                                        runMoveCommand a1

translate :: Translation -> (Moveable -> Moveable)
translate (Translation tr) (o@(Orientation q), Position3D pos) =
  (o, Position3D $ pos + L.rotate q tr)

rotate :: Rotation -> Degrees -> (Moveable -> Moveable)
rotate r (Degrees d) (Orientation q, p) =
  (Orientation $ q * L.axisAngle axis (deg2rad d), p)
  where axis = case r of Pitch -> L.V3 1 0 0
                         Yaw   -> L.V3 0 1 0
                         Roll  -> L.V3 0 0 (-1)

deg2rad :: RealFloat a => a -> a
deg2rad x = x * pi / 180
