{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses,
        OverloadedStrings #-}
module CommonGL where

import CommonAL
import Geometry
import Orientation
import Platform

import Control.Applicative
import Control.Arrow
import Control.Exception (evaluate)
import Control.Monad.Reader
import Control.Monad.Trans
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid hiding (All)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import FRP.Sodium
import Graphics.Rendering.OpenGL as GL hiding (Triangle, Rect, translate, normal)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.FTGL hiding (Font)
import qualified Graphics.Rendering.FTGL as FTGL
import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe
import System.Mem
import System.Random


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

-- | Get system time in seconds since the start of the Unix epoch
-- (1 Jan 1970).
getTime :: UTCTime -> IO Double
getTime t0 = do
    t <- getCurrentTime
    return $ realToFrac (t `diffUTCTime` t0)

glEngine :: Platform p =>
            ((Int -> Int -> FilePath -> FilePath -> Internals p -> IO (IO (), IO (), Touch p -> TouchPhase -> Coord -> Coord -> IO ())) -> IO ())
         -> Game p
         -> IO ()
glEngine initGraphics game = initGraphics $ \width height resourceDir stateDir internals -> do
    let aspect = fromIntegral width / fromIntegral height
    putStrLn $ "screen size:  "++show width++" x "++show height
    putStrLn $ "resource dir: "++resourceDir

    (eMouse, sendMouse) <- sync $ newEvent
    let touched touch phase x y = do
            let e = case phase of
                    TouchBegan     -> MouseDown touch (x,y)
                    TouchMoved     -> MouseMove touch (x,y)
                    TouchEnded     -> MouseUp   touch (x,y)
                    TouchCancelled -> MouseUp   touch (x,y)
            --print e
            sync $ sendMouse e

    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

    (time, sendTime) <- sync $ newBehavior 0
    (realTime, sendRealTime) <- sync $ newBehavior 0
    rng <- newStdGen
    (bSprite, bMusic, eEffects) <- sync $ game eMouse time rng
    spriteRef <- newIORef =<< sync (sample bSprite)
    kill1 <- sync $ listen (updates bSprite) (writeIORef spriteRef)

    let kill = kill1

    -- Distribute the effects alternately between two threads so two effects
    -- can play simultaneously.
    Just device <- alOpenDevice
    (bEffects1, bEffects2, bMusic') <- sync $ do
        (eEffects1, eEffects2) <- twoStreams eEffects
        b1 <- hold [] $ (:[]) <$> eEffects1
        b2 <- hold [] $ (:[]) <$> eEffects2
        bMusic' <- removeDuplicateMusic bMusic
        return (b1, b2, bMusic')
    audioThread device [(bMusic', 0.5), (bEffects1, 1), (bEffects2, 1)]

    t0 <- getCurrentTime
    tLastEndRef <- newIORef =<< getTime t0
    timeLostRef <- newIORef 0
    tLastGC <- newIORef 0

    let updateFrame = do
            --t <- getTime
            t <- readIORef tLastEndRef
            lost <- readIORef timeLostRef
            --putStrLn $ showFFloat (Just 3) (t - lost) ""
            sync $ do
                sendTime (t - lost)
                sendRealTime t
            sprite <- readIORef spriteRef
            preRunSprite internals height sprite
            tEnd <- getTime t0
            let lost = tEnd - t
            when (lost >= 0.1) $ do
                since <- (\last -> tEnd - last) <$> readIORef tLastGC
                if lost >= 0.25 && since >= 3 then do
                    --putStrLn "major GC"
                    performGC
                    tEnd' <- getTime t0
                    writeIORef tLastGC tEnd'
                    let lost' = tEnd' - t
                    modifyIORef timeLostRef (+lost')
                  else
                    modifyIORef timeLostRef (+lost)
                --putStrLn $ "PRE " ++ showFFloat (Just 3) lost ""
    let drawFrame = do
            tStart <- getTime t0
            GL.clear [ColorBuffer]
            loadIdentity
            GL.scale (0.001/realToFrac aspect) 0.001 (0.001 :: GLfloat)
            sprite <- readIORef spriteRef
            runSprite internals height sprite True
            tEnd <- getTime t0
            writeIORef tLastEndRef tEnd
            _ <- evaluate kill
            return ()

    updateFrame
    return (updateFrame, drawFrame, touched)

-- | Split the event into two alternating streams so it is possible to have two
-- effects playing at the same time.
twoStreams :: Event a -> Reactive (Event a, Event a)
twoStreams e = do
    ePair <- collectE (\a which ->
        (if which then (Just a, Nothing)
                  else (Nothing, Just a), not which)) False e
    let e1 = filterJust $ fst <$> ePair
        e2 = filterJust $ snd <$> ePair
    return (e1, e2)

removeDuplicateMusic :: Behavior (Text, [Sound p]) -> Reactive (Behavior [Sound p])
removeDuplicateMusic b = do
    delta <- collectE (\(new, music) old ->
       (if new /= old then Just music else Nothing, new)) "" (value b)
    hold [] $ filterJust delta

