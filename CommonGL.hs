{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}
module CommonGL where

import Geometry
import Orientation
import Platform

import Graphics.Rendering.OpenGL as GL hiding (Triangle, Rect, translate, normal)
import qualified Graphics.Rendering.OpenGL as GL
import Control.Applicative
import Control.Arrow
import Control.Monad.Reader
import Control.Monad.Trans
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid hiding (All)
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.Rendering.FTGL hiding (Font)
import qualified Graphics.Rendering.FTGL as FTGL
import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe


black :: Color4 GLfloat
black = Color4 0 0 0 1

red :: Color4 GLfloat
red = Color4 1 0 0 1

yellow :: Color4 GLfloat
yellow = Color4 0.992 0.996 0.262 1

white :: Color4 GLfloat
white = Color4 1 1 1 1

grey :: Color4 GLfloat
grey = Color4 0.5 0.5 0.5 1

appendKey :: Key -> Key -> Key
appendKey NullKey k = k
appendKey k NullKey = k
appendKey k1 k2 = CompositeKey k1 k2

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

nextPowerOf2 :: Int -> Int
nextPowerOf2 x = go 1
  where
    go p | x <= p = p
    go p = go (p+p)

standardVBox :: Ptr CFloat
{-# NOINLINE standardVBox #-}
standardVBox = unsafePerformIO $ do
    vBox <- mallocArray 12
    pokeArray (vBox :: Ptr CFloat) [
            -1,  1,
             1,  1,
            -1, -1,
             1, -1
        ]
    return vBox

standardTBox :: Ptr CFloat
{-# NOINLINE standardTBox #-}
standardTBox = unsafePerformIO $ do
    tBox <- mallocArray 8
    pokeArray (tBox :: Ptr CFloat) [
             0, 0,
             1, 0,
             0, 1,
             1, 1
        ]
    return tBox

normal :: Vector3 GLfloat
normal = Vector3 0 0 1

aspectify :: GLfloat -> IO ()
aspectify aspect | aspect < 1 = GL.scale 1 (1/aspect) 1
aspectify aspect              = GL.scale aspect 1 1

orient :: Orientation -> IO ()
orient OrientationUp            = return ()
orient OrientationDown          = GL.rotate 180 normal
orient OrientationLeft          = GL.rotate 90 normal
orient OrientationRight         = GL.rotate (-90) normal
orient OrientationUpMirrored    = GL.scale (-1) 1 (1 :: GLfloat)
orient OrientationDownMirrored  = GL.scale (-1) 1 (1 :: GLfloat) >> GL.rotate 180 normal
orient OrientationLeftMirrored  = GL.scale 1 (-1) (1 :: GLfloat) >> GL.rotate 90 normal
orient OrientationRightMirrored = GL.scale 1 (-1) (1 :: GLfloat) >> GL.rotate (-90) normal

drawBB :: IO ()
{-# INLINE drawBB #-}
drawBB = do
    arrayPointer VertexArray $= VertexArrayDescriptor 2 Float 0 standardVBox
    clientState VertexArray $= Enabled
    arrayPointer TextureCoordArray $= VertexArrayDescriptor 2 Float 0 standardTBox
    clientState TextureCoordArray $= Enabled
    drawArrays TriangleStrip 0 4
    texture Texture2D $= Disabled
    clientState VertexArray $= Disabled
    clientState TextureCoordArray $= Disabled

{-
colouredRectangle :: Platform p => Color4 GLfloat -> Drawable p
colouredRectangle (Color4 r g b _) = mkDrawable $ \brightness -> do
    color (Color4 r g b brightness)
    drawBB
-}

offscreen :: Rect
          -> Int
          -> Maybe (GLint -> GLint -> IO ()) -> (GLfloat -> IO ())
          -> IO ((Point, GLfloat) -> IO (), IO ())
offscreen rect@((posX, posY), size@(sizeX, sizeY)) screenHt multisample draw = do
    --putStrLn $ "cache "++show key++" "++show rect
    [fbo] <- genObjectNames 1
    bindFramebuffer DrawFramebuffer $= fbo

    let pxScale = realToFrac screenHt / 1000 
        width   = (ceiling $ sizeX * pxScale :: Int)
        height  = (ceiling $ sizeY * pxScale :: Int)
        pixels  = nextPowerOf2 (max width height)

    [storage] <- genObjectNames 1

    [to] <- genObjectNames 1
    textureBinding Texture2D $= Just to
    textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
    textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
    textureFilter Texture2D $= if isJust multisample then ((Linear', Nothing), Linear')
                                                     else ((Nearest, Nothing), Nearest)
    --print (width, height, scale, pixels, text)
    let drawOutline = False
    texImage2D Texture2D NoProxy 0
        (if drawOutline then RGB' else RGBA')
        (TextureSize2D (fi pixels) (fi pixels)) 0 (PixelData
            (if drawOutline then RGB else RGBA) UnsignedByte nullPtr)
    textureBinding Texture2D $= Nothing

    if isJust multisample then do
        bindRenderbuffer Renderbuffer $= storage
        let sz = RenderbufferSize (fi pixels) (fi pixels)
        renderbufferStorageMultiSample Renderbuffer (Samples 4) RGBA8 sz
        framebufferRenderbuffer Framebuffer (ColorAttachment 0) Renderbuffer storage
      else
        framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D to 0
    status <- get (framebufferStatus Framebuffer)
    case status of
        Complete -> return ()
        _ -> putStrLn "framebuffer error!"
    savedVP <- get viewport
    viewport $= (Position 0 0, Size (fi width) (fi height))
    preservingMatrix $ do
        GL.clear [ColorBuffer]
        loadIdentity
        GL.scale (realToFrac $ 1/sizeX) (realToFrac $ -1/sizeY) (1 :: GLfloat)
        GL.translate $ Vector3 (realToFrac $ -posX) (realToFrac $ -posY) (0 :: GLfloat)
        draw 1  -- always draw with 100% brightness
    viewport $= savedVP

    case multisample of
        Just resolve -> do
            [fbo2] <- genObjectNames 1
            bindFramebuffer DrawFramebuffer $= fbo2
            framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D to 0
            bindFramebuffer ReadFramebuffer $= fbo
            resolve (fi pixels) (fi pixels)
            deleteObjectNames [fbo2]
        Nothing -> return ()

    bindFramebuffer DrawFramebuffer $= defaultFramebufferObject
    deleteObjectNames [fbo]
    deleteObjectNames [storage]
    let scaleX = realToFrac $ sizeX * fi pixels / fi width
        scaleY = realToFrac $ sizeY * fi pixels / fi height
        shiftX = realToFrac $ fi (pixels - width) / fi pixels
        shiftY = realToFrac $ fi (height - pixels) / fi pixels
    let draw ((posX, posY), brightness) = preservingMatrix $ do
            textureBinding Texture2D $= Just to
            texture Texture2D $= Enabled
            GL.translate $ Vector3 (realToFrac $ posX) (realToFrac $ posY) (0 :: GLfloat)
            GL.scale scaleX scaleY (1 :: GLfloat)
            GL.translate $ Vector3 shiftX shiftY (0 :: GLfloat)
            GL.color $ Color4 1 1 1 brightness
            drawBB
    return (draw, deleteObjectNames [to])

glLabel :: FTGL.Font
        -> Float        -- Y correction upwards, %
        -> Rect
        -> Color4 GLfloat
        -> Text
        -> IO ()
glLabel font ycorr rect@((posX, posY),sz@(sizeX, sizeY)) colour text = do
    let stext = T.unpack text
    (scale, w, h) <- do
        setFontFaceSize font 1000 0
        [x0,y0,_,x1,y1,_] <- getFontBBox font stext
        let asc = getFontAscender font
        let desc = getFontDescender font
        adv <- getFontAdvance font stext
        let width = sizeX * 2
            height = sizeY * 2
            width1000 = adv
            height1000 = (y1 - y0) + asc - desc
            scaleW = 1000 * realToFrac width / width1000 :: Float
            scaleH = 1000 * realToFrac height / height1000 :: Float
            scale = min scaleW scaleH
            w = scale * width1000 / 1000
            h = scale * height1000 / 1000
        return (floor (scale * 0.995) :: Int, w :: Float, h :: Float)
    setFontFaceSize font scale 72
    [x0,y0,_,x1,y1,_] <- getFontBBox font stext
    let asc = getFontAscender font
        desc = getFontDescender font
        dx = realToFrac $ -(x1 - x0) / 2
        dy = realToFrac $ (- asc) * 0.5 + (asc) * ycorr
    GL.translate $ Vector3 dx dy (0 :: GLfloat)
    color colour
    renderFont font stext All

