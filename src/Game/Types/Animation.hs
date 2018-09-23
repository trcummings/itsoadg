{-# LANGUAGE LambdaCase #-}

module Game.Types.Animation where

import qualified Animate as A
import           Data.Text (Text)

import           Game.Types.Components (Texture(..))
import           Game.Types.Util       (Seconds(..))

data AnimationKey =
    PlayerKey'Walk
  | PlayerKey'Idle
  | EnemyKey'Walk
  | EnemyKey'Idle
  deriving (Show, Eq, Ord, Bounded, Enum)

instance A.KeyName AnimationKey where
  keyName = animKey'keyName

animKey'keyName :: AnimationKey -> Text
animKey'keyName = \case
  PlayerKey'Walk -> "WalkFront"
  PlayerKey'Idle -> "IdleFront"
  EnemyKey'Walk  -> "WalkBack"
  EnemyKey'Idle  -> "IdleBack"

-- type Animations key = A.Animations key (A.SpriteClip key) Seconds
type AnimationClip = A.SpriteClip AnimationKey
type SheetInfo = A.SpriteSheet AnimationKey Texture Seconds
type FrameInfo = A.Position    AnimationKey Seconds

data SpriteSheet = SpriteSheet
  { _ssSheet    :: SheetInfo
  , _ssPosition :: FrameInfo }
