module Util.RaycastSpec where

import           Linear (V2(..), (*^))

import           Game.Util.RayCast (checkIntersection, castRayOnBox)
import           Game.Types
  ( AABB(..)
  , RaycastHit(..)
  , Unit(..)
  , Position(..) )

spec :: Spec
spec = do
  describe "Game.Util.Raycast" $ do
    context "rayIntersection2d" $ do
      return ()

main :: IO ()
main = hspec spec
