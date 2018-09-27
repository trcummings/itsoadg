module Game.Types.Font where

import Linear   (V2)
import Data.Map (Map)

import Game.Types.Components (Texture)


-- Font types
data DebugHUD = DebugHUD
 { _hudInfo :: HUDInfo
 , _fontMap :: FontMap }

data HUDType =
    FPSCounter
  | PositionTracker
  | PlayerFacing
  deriving (Eq, Ord)

newtype HUDInfo = HUDInfo (Map HUDType FontInfo)
newtype FontMap = FontMap (Map Char    Character)

data FontInfo = FontInfo
  { _fText :: String
  , _fxPos :: Float
  , _fyPos :: Float
  , _fSize :: Float }

data Character = Character
  -- texture object for character
  { _charTexture :: Texture
  -- size of glyph
  -- , _charSize    :: L.V2 Float
  -- offset from baseline to left/top of glyph
  , _charBearing :: V2 Float
  -- offset to advance to next glyph
  , _charAdvance :: Int }
  deriving Show