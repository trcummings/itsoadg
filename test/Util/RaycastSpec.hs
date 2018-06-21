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
      it "returns a hit when the ray just touches it" $ do
        let ray = Ray  { origin = V2 0.5 (1 + onePixel)  , delta = V2 0 1 }
            box = AABB { center = V2 0.5 2.5, dims  = V2 1 1 }
            hit = rayIntersection2d ray box
        (distance <$> hit) `shouldBe` (Just $ V2 0 (1 - onePixel))
      it "returns the closest hit when the ray is longer than the box" $ do
        let ray = Ray  { origin = V2 0.5 (1 + onePixel)  , delta = V2 0 5 }
            box = AABB { center = V2 0.5 2.5, dims  = V2 1 1 }
            hit = rayIntersection2d ray box
        (distance <$> hit) `shouldBe` (Just $ V2 0 (1 - onePixel))

main :: IO ()
main = hspec spec
