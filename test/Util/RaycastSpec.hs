module Util.RaycastSpec where

import           Test.Hspec
import           Test.Hspec.Expectations
import           Linear (V2(..), (*^))
import           Control.Monad.IO.Class (liftIO)

import           Game.Util.Raycast (rayIntersection2d)
import           Game.Util.Constants (onePixel)
import           Game.Types
  ( AABB(..)
  , Ray(..)
  , RaycastHit(..)
  , Unit(..)
  , Position(..) )

spec :: Spec
spec = do
  describe "Game.Util.Raycast" $ do
    context "rayIntersection2d" $ do
      let tinyRay  = Ray  { origin = V2 0.5 (1 + onePixel)  , delta = V2 0 0.5 }
          shortRay = Ray  { origin = V2 0.5 (1 + onePixel)  , delta = V2 0 1 }
          longRay  = Ray  { origin = V2 0.5 (1 + onePixel)  , delta = V2 0 5 }
          box      = AABB { center = V2 0.5 2.5, dims = V2 1 1 }
      it "returns a hit when the ray just touches it" $ do
        let hit = rayIntersection2d shortRay box
        (distance <$> hit) `shouldBe` (Just $ V2 0 (1 - onePixel))
        (normal <$> hit)   `shouldBe` (Just $ V2 0 1)
      it "returns the closest hit when the ray is longer than the box" $ do
        let hit = rayIntersection2d longRay box
        (distance <$> hit) `shouldBe` (Just $ V2 0 (1 - onePixel))
        (normal <$> hit)   `shouldBe` (Just $ V2 0 1)
      it "returns nothing when the ray is too short to hit the box" $ do
        let hit = rayIntersection2d tinyRay box
        (distance <$> hit) `shouldBe` Nothing
        (normal <$> hit)   `shouldBe` Nothing

main :: IO ()
main = hspec spec
