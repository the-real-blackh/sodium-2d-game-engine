{-# LANGUAGE ForeignFunctionInterface, TypeFamilies, FlexibleInstances,
        MultiParamTypeClasses, OverloadedStrings, Rank2Types #-}
module GLUT where

import CommonAL (SoundInfo(..))
import qualified CommonAL as CommonAL
import CommonGL
import Geometry
import Image
import Platform

import Control.Applicative
import Control.Concurrent (threadDelay, forkIO)
import Control.Exception (evaluate)
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Time.Clock
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid (Monoid(..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import FRP.Sodium
import System.FilePath
import Foreign hiding (unsafePerformIO)
import Foreign.C
import qualified Graphics.Rendering.FTGL as FTGL
import Graphics.Rendering.OpenGL.GL as GL hiding (Rect, normal)
import Graphics.Rendering.OpenGL.Raw
import qualified Graphics.UI.GLUT as GLUT hiding (Rect, translate)
import System.Exit
import System.FilePath
import System.IO.Unsafe
import System.Mem
import System.Random (newStdGen)


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
    bSprite <- sync $ game eMouse time rng
    spriteRef <- newIORef =<< sync (sample bSprite)
    kill1 <- sync $ listen (updates bSprite) (writeIORef spriteRef)

    let kill = kill1

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

initGraphics :: Args GLUT
             -> (Int -> Int -> FilePath -> FilePath -> Internals GLUT -> IO (IO (), IO (), Touch GLUT -> TouchPhase -> Coord -> Coord -> IO ()))
             -> IO ()
initGraphics (GLUTArgs title) init = do
    _ <- GLUT.getArgsAndInitialize
    GLUT.initialDisplayMode $= [GLUT.DoubleBuffered]
    GLUT.createWindow title
    let width = 960
        height = 640
        resourceDir = "."
        aspect = realToFrac width / realToFrac height
    GLUT.windowSize $= GLUT.Size (fromIntegral width) (fromIntegral height)
    cache <- newCache height
    let internals = GLUTInternals {
                inCache = cache
            }
    (updateFrame, drawFrame, touched) <- init width height resourceDir resourceDir internals

    GLUT.displayCallback $= display updateFrame drawFrame
    GLUT.addTimerCallback (1000 `div` frameRate) $ repaint

    let motion (GLUT.Position x y) = do
            (x', y') <- toScreen x y
            touched () TouchMoved x' y'
    GLUT.motionCallback $= Just motion
    GLUT.passiveMotionCallback $= Just motion
    GLUT.keyboardMouseCallback $= Just (\key keyState mods pos -> do
        case (key, keyState, pos) of
            (GLUT.MouseButton GLUT.LeftButton, GLUT.Down, GLUT.Position x y) -> do
                (x', y') <- toScreen x y
                touched () TouchBegan x' y'
            (GLUT.MouseButton GLUT.LeftButton, GLUT.Up,   GLUT.Position x y) -> do
                (x', y') <- toScreen x y
                touched () TouchEnded x' y'
            (GLUT.MouseButton GLUT.MiddleButton, GLUT.Down, GLUT.Position x y) ->
                exitSuccess
            _ -> return ()
      )
    
    GLUT.mainLoop

  where
    toScreen :: GLint -> GLint -> IO (Coord, Coord)
    toScreen x y = do
        (_, Size w h) <- get viewport
        let aspect = fromIntegral w / fromIntegral h
            sx = 0.001/aspect
            sy = 0.001
            xx = 2 * ((fromIntegral x / fromIntegral w) - 0.5) / sx
            yy = 2 * (0.5 - (fromIntegral y / fromIntegral h)) / sy
        return (xx, yy)
    repaint = do
        GLUT.postRedisplay Nothing
        GLUT.addTimerCallback (1000 `div` frameRate) $ repaint

    display :: IO () -> IO () -> IO ()
    display updateFrame drawFrame = do
        updateFrame
        drawFrame
        GLUT.swapBuffers

data SpriteState = SpriteState {
        ssFont       :: Font GLUT,
        ssInternals  :: Internals GLUT,
        ssBrightness :: GLfloat
    }

data GLUT = GLUT
    {
        resKey  :: Key,
        resDraw :: GLfloat -> IO ()
    }

instance Monoid (Sprite GLUT) where
    mempty = Sprite NullKey ((0,0),(0,0)) Nothing $ return ()
    Sprite ka ra ca a `mappend` Sprite kb rb cb b =
        Sprite (ka `appendKey` kb) (ra `appendRect` rb) (ca `appendCache` cb) (a >> b)

appendCache :: Maybe (ReaderT SpriteState IO ()) -> Maybe (ReaderT SpriteState IO ()) -> Maybe (ReaderT SpriteState IO ())
appendCache (Just a)    (Just b) = Just (a >> b)
appendCache ja@(Just _) _        = ja
appendCache _           mb       = mb

frameRate :: Num a => a
frameRate = 30

simulateIOSSpeed = True

drawAt :: Key -> (GLfloat -> IO ()) -> Rect -> Sprite GLUT
drawAt key action rect@((posX, posY),(sizeX, sizeY)) = Sprite key rect Nothing $ do
    brightness <- asks ssBrightness
    liftIO $ preservingMatrix $ do
        GL.translate $ Vector3 (realToFrac posX) (realToFrac posY) (0 :: GLfloat)
        GL.scale (realToFrac sizeX) (realToFrac sizeY) (1 :: GLfloat)
        action brightness

instance Platform GLUT where
    data Args GLUT = GLUTArgs {
            gaTitle :: String
        }
    data Internals GLUT = GLUTInternals {
            inCache :: Cache
        }
    data Sprite GLUT = Sprite {
            spKey   :: Key,
            spRect  :: Rect,  -- Bounding box
            spCache :: Maybe (ReaderT SpriteState IO ()),
            spDraw  :: ReaderT SpriteState IO ()
        }
    data Font GLUT = Font {
            ftFont :: FTGL.Font,
            ftYCorr :: Float
        }
    newtype Sound GLUT = Sound (IORef SoundInfo)
    type Touch GLUT = ()

    engine args game = glEngine (initGraphics args) game

    nullDrawable = drawAt NullKey $ \_ -> return ()

    image resDir path = do
        let key = ByteStringKey $ C.pack $ takeFileName path
        return $ \rect@((posX, posY),(sizeX, sizeY)) ->
            let cacheIt = Just $ do
                    cache <- asks (inCache . ssInternals)
                    liftIO $ writeCache cache key $ do
                        putStrLn $ "loading "++path
                        when simulateIOSSpeed $ liftIO $ threadDelay 600
                        ti <- loadTexture path False
                        to <- createTexture ti
                        let draw' _ brightness = do
                                color $ Color4 1 1 1 brightness
                                textureBinding Texture2D $= Just to
                                texture Texture2D $= Enabled
                                textureFilter Texture2D $= ((Linear', Nothing), Linear')
                                drawBB
                            cleanup' = do
                                --putStrLn $ "unloading "++path
                                deleteObjectNames [to]
                        return (draw', cleanup')
                drawIt = do
                    cache <- asks (inCache . ssInternals)
                    brightness <- asks ssBrightness
                    liftIO $ preservingMatrix $ do
                        GL.translate $ Vector3 (realToFrac posX) (realToFrac posY) (0 :: GLfloat)
                        GL.scale (realToFrac sizeX) (realToFrac sizeY) (1 :: GLfloat)
                        mDraw <- readCache cache key
                        case mDraw of
                            Just draw -> draw (posX, posY) brightness
                            Nothing   -> return ()
            in  Sprite key rect cacheIt drawIt

    sound resDir file = Sound <$> newIORef (SoundPath $ resDir </> file)
    retainSound (Sound siRef) = do
        si <- readIORef siRef
        si' <- CommonAL.loadSound si
        writeIORef siRef si'

    translateSprite v@(vx, vy) (Sprite key rect cache action) = Sprite key (translateRect v rect) cache $ do
        r <- ask
        liftIO $ preservingMatrix $ do
            GL.translate $ Vector3 (realToFrac vx) (realToFrac vy) (0 :: GLfloat)
            runReaderT action r
    createFont resPath ycorr = Font <$> FTGL.createPolygonFont resPath <*> pure ycorr 
    uncachedLabel rect@((posX, posY), _) (Color4 r g b _) text = Sprite (TextKey text) rect Nothing $ do
        when simulateIOSSpeed $ liftIO $ threadDelay 10000
        ft <- asks ssFont
        internals <- asks ssInternals
        brightness <- asks ssBrightness
        liftIO $ preservingMatrix $ do
            GL.translate $ Vector3 (realToFrac posX) (realToFrac posY) (0 :: GLfloat)
            glLabel (ftFont ft) (ftYCorr ft) rect (Color4 r g b brightness) text
    key k s = s { spKey = k }
    keyOf d = spKey $ d ((0,0),(0,0))
    setBoundingBox r s = s { spRect = r }
    cache toMultisample (Sprite key rect@(pos, _) cache1 action) =
        let cache2 = do
                ss <- ask
                let draw brightness = runReaderT (do
                        when simulateIOSSpeed $ liftIO $ threadDelay 600
                        action
                      ) ss { ssBrightness = brightness }
                    cache = inCache . ssInternals $ ss
                    multisample = if toMultisample
                        then Just $ \w h -> blitFramebuffer (Position 0 0) (Position w h)
                                                            (Position 0 0) (Position w h)
                                                            [ColorBuffer']
                                                            Nearest
                        else Nothing
                liftIO $ writeCache cache key $ offscreen rect (ccScreenHt cache) multisample draw
                return ()
            draw = do
                ss <- ask
                let cache = inCache . ssInternals $ ss
                liftIO $ do
                    mDraw' <- readCache cache key
                    case mDraw' of
                        Just draw' -> draw' pos (ssBrightness ss)
                        Nothing    -> return ()
        in Sprite key rect (cache1 `appendCache` Just cache2) draw
    fade brightness (Sprite key rect cache action) = Sprite key rect cache $ do
        ss <- ask
        liftIO $ runReaderT action (ss { ssBrightness = ssBrightness ss * brightness })
    shrink factor (Sprite key rect@((posX,posY),_) cache action) = Sprite key rect cache $ do
        ss <- ask
        let px = realToFrac posX
            py = realToFrac posY
        liftIO $ preservingMatrix $ do
            GL.translate $ Vector3 px py (0 :: GLfloat)
            GL.scale factor factor 1
            GL.translate $ Vector3 (-px) (-py) (0 :: GLfloat)
            runReaderT action ss
    preRunSprite internals brightness (Sprite _ _ mCache action) = do
        let ss = SpriteState {
                    ssFont = error "font not defined!",
                    ssInternals = internals,
                    ssBrightness = 1
                }
        case mCache of
            Just cache -> runReaderT cache ss
            Nothing    -> return ()
    runSprite internals brightness (Sprite _ _ mCache action) flip = do
        let ss = SpriteState {
                    ssFont = error "font not defined!",
                    ssInternals = internals,
                    ssBrightness = 1
                }
        runReaderT action ss
        when flip $ flipCache (inCache internals)

    audioThread device bSounds = CommonAL.audioThread device $
        (\(b, gain) -> (map (\(Sound si) -> si) <$> b, realToFrac gain)) <$> bSounds

    -- Rotate the sprite 90 degrees clockwise
    clockwiseSprite (Sprite key rect cache action) = Sprite key (clockwiseRect rect) cache $ do
        ss <- ask
        liftIO $ preservingMatrix $ do
            GL.rotate 90 normal
            runReaderT action ss

    -- Rotate the sprite 90 degrees anti-clockwise
    anticlockwiseSprite (Sprite key rect cache action) = Sprite key (anticlockwiseRect rect) cache $ do
        ss <- ask
        liftIO $ preservingMatrix $ do
            GL.rotate (-90) normal
            runReaderT action ss

    rotateSprite theta (Sprite key rect@((posX, posY), _) cache action) = Sprite key rect cache $ do
        ss <- ask
        let px = realToFrac posX
            py = realToFrac posY
        liftIO $ preservingMatrix $ do
            GL.translate $ Vector3 px py (0 :: GLfloat)
            GL.rotate theta normal
            GL.translate $ Vector3 (-px) (-py) (0 :: GLfloat)
            runReaderT action ss

    -- Make this sprite invisible, but allow it to cache anything in the invisible sprite
    invisible (Sprite key rect cache _) = Sprite key rect cache (return ())

    launchURL _ url = C.putStrLn $ "launch url: " `mappend` url
    
    getSystemLanguage _ = return "en"

