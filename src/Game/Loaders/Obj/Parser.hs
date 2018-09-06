module Game.Loaders.Obj.Parser (runObjParser) where

import           Graphics.Rendering.OpenGL (GLfloat)
import           Control.Applicative       ((*>), (<|>), liftA2, many)
import           Data.ByteString.Char8     (ByteString)
import           Linear                    (V2(..), V3(..))
import qualified Data.Attoparsec.ByteString.Char8 as A (takeWhile)
import           Data.Attoparsec.ByteString.Char8
  ( Parser
  , skipSpace
  , endOfLine
  , parseOnly
  , double
  , decimal
  , char )

import           Game.Loaders.Obj.Types (ObjLine(..))


runObjParser :: ByteString -> [ObjLine]
runObjParser contents =
  let Right results = parseOnly parseObj contents
  in results

-- parse functions
parseObj :: Parser [ObjLine]
parseObj = many $ parseObjLine <* endOfLine

parseObjLine :: Parser ObjLine
parseObjLine =
        parseVertLine
    <|> parseNormLine
    <|> parseTexLine
    <|> parseFaceLine
    <|> parseInvalid


parseVertLine :: Parser ObjLine
parseVertLine = char 'v' *> skipSpace *> (LineVert <$> parseV3)

parseNormLine :: Parser ObjLine
parseNormLine = "vn" *> skipSpace *> (LineNorm <$> parseV3)

parseTexLine :: Parser ObjLine
parseTexLine = "vt" *> skipSpace *> (LineTex . texCoordToGLFormat <$> parseV2)

parseFaceLine :: Parser ObjLine
parseFaceLine = char 'f' *> skipSpace *> (LineFace <$> parseFaceDat)

parseFaceDat :: Parser (V3 (V3 Int))
parseFaceDat = toV3 <$> timesSep 3 parseFaceGroup skipSpace
  where
    toV3 :: [V3 Int] -> V3 (V3 Int)
    toV3 [x, y, z] = V3 x y z
    toV3 _ = error ("Game.Loaders.Obj.Parser." ++
                    "parseFaceDat.toV3 - Bad list length!")

parseFaceGroup :: Parser (V3 Int)
parseFaceGroup = do
    v <- decimal
    _ <- char '/'
    vt <- decimal <|> return (-1)
    _ <- char '/'
    vn <- decimal <|> return (-1)
    return $ V3 v vt vn

parseV3 :: Parser (V3 GLfloat)
parseV3 = do
  V2 x y <- parseV2
  skipSpace
  z <- realToFrac <$> double
  return $ V3 x y z

parseV2 :: Parser (V2 GLfloat)
parseV2 = do
  x <- realToFrac <$> double
  skipSpace
  y <- realToFrac <$> double
  return $ V2 x y

parseInvalid :: Parser ObjLine
parseInvalid = Invalid <$> A.takeWhile (/='\n')

-- helper functions

-- | .obj files store indices starting at 1, but our indexing starts at
--   0, so we need to adjust them
texCoordToGLFormat :: V2 GLfloat -> V2 GLfloat
texCoordToGLFormat (V2 x y) = V2 x (1 - y)

-- | This MUST be NOINLINE'd if -O2 is used,
--   otherwise it crashes, due to a GHC 7.8.2
--   bug.
timesSep :: Int -> Parser a -> Parser b -> Parser [a]
timesSep = times' 0
  where
    times' i total parser sep
      | i + 1 <  total = liftA2 (:) (parser <* sep) (times' (i + 1) total parser sep)
      | i + 1 == total = liftA2 (:) parser (times' (i + 1) total parser sep)
      | otherwise = return []

-- fromJustSafe :: Num a => Maybe a -> a
-- fromJustSafe (Just x) = x
-- fromJustSafe Nothing = 0
