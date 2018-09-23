module Game.Loaders.Font (loadCharacters) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear                    as L
import           SDL              (($=))
import           Control.Monad    (unless, fail, forM, mapM)
import           Data.Map         (fromList)
import           System.IO        (hPutStrLn, stderr)
import           Foreign          (alloca, peek, plusPtr)
import           Foreign.C.String (withCString)
import           Graphics.Rendering.FreeType.Internal.Library    (FT_Library)
import           Graphics.Rendering.FreeType.Internal.BitmapSize (FT_Bitmap_Size)
import           Graphics.Rendering.FreeType.Internal.Vector     (FT_Vector(x))
import           Graphics.Rendering.FreeType.Internal
  ( ft_Init_FreeType
  , ft_New_Face
  , ft_Render_Glyph
  , ft_Load_Glyph
  , ft_Get_Char_Index
  , ft_Set_Pixel_Sizes
  , ft_Done_Face
  , ft_Done_FreeType
  )
import           Graphics.Rendering.FreeType.Internal.Face
  ( FT_Face
  , available_sizes
  , num_fixed_sizes
  , num_glyphs
  , glyph
  )
import           Graphics.Rendering.FreeType.Internal.GlyphSlot
  ( FT_GlyphSlot
  , bitmap
  , bitmap_top
  , bitmap_left
  , advance
  , format
  )
import           Graphics.Rendering.FreeType.Internal.Bitmap
  ( pixel_mode
  , num_grays
  , palette_mode
  , buffer
  , rows
  , width
  , pitch
  )
import           Graphics.Rendering.FreeType.Internal.PrimitiveTypes
  ( FT_Glyph_Format
  , FT_Error
  , ft_GLYPH_FORMAT_COMPOSITE
  , ft_GLYPH_FORMAT_OUTLINE
  , ft_GLYPH_FORMAT_PLOTTER
  , ft_GLYPH_FORMAT_BITMAP
  , ft_RENDER_MODE_NORMAL
  )

import Game.Util.GLError (printGLErrors)
import Game.Types        (Character(..), Texture(..), FontMap(..))

loadCharacters :: FilePath
               -> Int
               -> GL.TextureUnit
               -> String
               -> IO FontMap
loadCharacters path pxWidth texUnit chars = do
    -- Set the texture params on our bound texture.
    GL.activeTexture          $= texUnit
    printGLErrors "Game.Loaders.Font.loadCharacter, GL.activeTexture"

    -- Set the alignment to 1 byte, disabling the byte-alignment
    -- restriction that would otherwise make this maybe segfault
    GL.rowAlignment GL.Unpack $= 1
    printGLErrors "Game.Loaders.Font.loadCharacter, GL.rowAlignment"

    GL.activeTexture          $= texUnit
    printGLErrors "Game.Loaders.Font.loadCharacter, GL.activeTexture"

    -- FreeType (http://freetype.org/freetype2/docs/tutorial/step1.html)
    -- create handle to library object
    ft <- freeType
    -- Create handle to face object
    ff <- fontFace ft path
    -- set face size
    runFreeType $ ft_Set_Pixel_Sizes ff 0 (fromIntegral pxWidth)

    -- Get the GlyphSlot.
    slot <- peek $ glyph ff
    -- print out number of glyphs
    n <- peek $ num_glyphs ff
    putStrLn $ "number of glyphs:" ++ show n
    --
    fmt <- peek $ format slot
    putStrLn $ "glyph format:" ++ glyphFormatString fmt

    -- This is [] for Ubuntu Mono, but I'm guessing for bitmap
    -- fonts this would be populated with the different font
    -- sizes.
    putStr "Sizes:"
    numSizes <- peek $ num_fixed_sizes ff
    sizesPtr <- peek $ available_sizes ff
    sizes    <- forM [0..(numSizes - 1)] $ \i ->
      peek $ sizesPtr `plusPtr` fromIntegral i :: IO FT_Bitmap_Size
    print sizes

    -- create all characters
    charMap <- mapM (loadCharacter ff slot) chars

    -- clean up freetype
    ft_Done_Face     ff
    ft_Done_FreeType ft

    -- return font map
    return $ FontMap $ fromList charMap

loadCharacter :: FT_Face -> FT_GlyphSlot -> Char -> IO (Char, Character)
loadCharacter ff slot char = do
    putStrLn $ "Loading glyph for letter \"" ++ [char] ++ "\""
    -- Get the unicode char index.
    chNdx <- ft_Get_Char_Index ff $ fromIntegral $ fromEnum char

    -- Load the glyph at the specified index into freetype memory.
    runFreeType $ ft_Load_Glyph ff chNdx 0

    -- convert glyph into a bitmap
    runFreeType $ ft_Render_Glyph slot ft_RENDER_MODE_NORMAL

    -- get the bitmap glyph top & left coordinates
    left <- peek $ bitmap_left slot
    top  <- peek $ bitmap_top  slot
    putStrLn $ concat
      [ "left:"
      , show left
      , "\ntop:"
      , show top
      ]

    -- Get the char bitmap.
    bmp <- peek $ bitmap slot
    -- get glyph slot advance
    adv <- peek $ advance slot
    putStrLn $ concat
      [ "width:"
      , show $ width bmp
      , " rows:"
      , show $ rows bmp
      , " pitch:"
      , show $ pitch bmp
      , " num_grays:"
      , show $ num_grays bmp
      , " pixel_mode:"
      , show $ pixel_mode bmp
      , " palette_mode:"
      , show $ palette_mode bmp
      , " advance:"
      , show adv
      ]

    let w         = fromIntegral $ width bmp
        h         = fromIntegral $ rows bmp
        w'        = fromIntegral w
        h'        = fromIntegral h
        advanceX  = fromIntegral $ x adv
        size      = GL.TextureSize2D w' h'
        pixelData = GL.PixelData GL.Red GL.UnsignedByte $ buffer bmp

    -- Generate an opengl texture.
    [tex] <- GL.genObjectNames 1
    GL.textureBinding GL.Texture2D $= Just tex
    printGLErrors "Game.Loaders.Font.loadCharacter, GL.textureBinding"

    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToEdge)
    printGLErrors "Game.Loaders.Font.loadCharacter, GL.textureWrapMode"

    putStrLn "Buffering glyph bitmap into texture."
    GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.R8 size 0 pixelData
    printGLErrors "Game.Loaders.Font.loadCharacter, GL.texImage2D"
    putStrLn "Texture loaded."

    GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
    printGLErrors "Game.Loaders.Font.loadCharacter, GL.textureFilter"

    GL.generateMipmap' GL.Texture2D
    printGLErrors "Game.Loaders.Font.loadCharacter, GL.generateMipmap'"

    -- return char, character map
    return
      ( char
      , Character { _charTexture = Texture { _textureId   = Just tex
                                           , _textureSize = size }
                  -- , _charSize    = L.V2 (realToFrac w)    (realToFrac h)
                  , _charBearing = L.V2 (realToFrac left) (realToFrac top)
                  , _charAdvance = advanceX }
      )


addPadding :: Int -> Int -> a -> [a] -> [a]
addPadding _ _ _ [] = []
addPadding amt w val xs = a ++ b ++ c
    where a = take w xs
          b = replicate amt val
          c = addPadding amt w val (drop w xs)


glyphFormatString :: FT_Glyph_Format -> String
glyphFormatString fmt
    | fmt == ft_GLYPH_FORMAT_COMPOSITE = "ft_GLYPH_FORMAT_COMPOSITE"
    | fmt == ft_GLYPH_FORMAT_OUTLINE   = "ft_GLYPH_FORMAT_OUTLINE"
    | fmt == ft_GLYPH_FORMAT_PLOTTER   = "ft_GLYPH_FORMAT_PLOTTER"
    | fmt == ft_GLYPH_FORMAT_BITMAP    = "ft_GLYPH_FORMAT_BITMAP"
    | otherwise                        = "ft_GLYPH_FORMAT_NONE"


runFreeType :: IO FT_Error -> IO ()
runFreeType m = do
    r <- m
    unless (r == 0) $ fail $ "FreeType Error:" ++ show r


freeType :: IO FT_Library
freeType = alloca $ \p -> do
    runFreeType $ ft_Init_FreeType p
    peek p


fontFace :: FT_Library -> FilePath -> IO FT_Face
fontFace ft fp = withCString fp $ \str ->
    alloca $ \ptr -> do
        runFreeType $ ft_New_Face ft str 0 ptr
        peek ptr
