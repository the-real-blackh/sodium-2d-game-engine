{-# LANGUAGE ForeignFunctionInterface, TypeFamilies, FlexibleInstances,
        MultiParamTypeClasses, OverloadedStrings, Rank2Types, EmptyDataDecls,
        JavaScriptFFI, RecursiveDo #-}
module FRP.Sodium.GameEngine2D.WebGL where

import FRP.Sodium.GameEngine2D.Cache
import FRP.Sodium.GameEngine2D.Geometry
import FRP.Sodium.GameEngine2D.Platform

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as C
import Data.IORef
import Data.Monoid
import Data.String
import qualified Data.Text as T
import Data.Time.Clock
import FRP.Sodium
import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Marshal
import Numeric
import System.FilePath
import System.Random (newStdGen)


data WebGL

appendCache :: Maybe (ReaderT SpriteState IO ()) -> Maybe (ReaderT SpriteState IO ()) -> Maybe (ReaderT SpriteState IO ())
appendCache (Just a)    (Just b) = Just (a >> b)
appendCache ja@(Just _) _        = ja
appendCache _           mb       = mb

instance Monoid (Sprite WebGL) where
    mempty = Sprite NullKey ((0,0),(0,0)) Nothing $ return ()
    Sprite ka ra ca a `mappend` Sprite kb rb cb b =
        Sprite (ka `appendKey` kb) (ra `appendRect` rb) (ca `appendCache` cb) (a >> b)

data GL_
type GL = JSRef GL_

foreign import javascript unsafe "initGL($1,$2,$3,$4)"
    initGL :: Element
           -> JSFun (JSRef Float -> JSRef Float -> IO ())
           -> JSFun (JSRef Float -> JSRef Float -> IO ())
           -> JSFun (JSRef Float -> JSRef Float -> IO ())
           -> IO GL

data Element_
type Element = JSRef Element_

foreign import javascript unsafe "document.getElementById($1)"
    getElementById :: JSString -> IO Element

foreign import javascript unsafe "$1.clearColor($2, $3, $4, $5);"
    clearColor :: GL -> Float -> Float -> Float -> Float -> IO ()

foreign import javascript unsafe "$1.clear($1.COLOR_BUFFER_BIT | $1.DEPTH_BUFFER_BIT);"
    clear :: GL -> IO ()

foreign import javascript unsafe "requestAnimFrame($1)"
    requestAnimFrame :: JSFun (IO ()) -> IO ()

foreign import javascript unsafe "window.addEventListener('resize',$1);"
    onWindowResize :: JSFun (IO ()) -> IO ()

{-
foreign import javascript unsafe "getWindowWidth()"
    windowWidth :: IO Float

foreign import javascript unsafe "getWindowHeight()"
    windowHeight :: IO Float
    -}

foreign import javascript unsafe "$1.offsetWidth"
    canvasWidth :: Element -> IO Float

foreign import javascript unsafe "$1.offsetHeight"
    canvasHeight :: Element -> IO Float

foreign import javascript unsafe "resizeViewport($1,$2,$3)"
    resizeViewport :: Element -> Float -> Float -> IO ()

data Texture_
type Texture = JSRef Texture_

foreign import javascript unsafe "loadImage"
    loadImage :: JSString -> IO Texture

foreign import javascript unsafe "drawImage"
    drawImage :: Texture -> Float -> Float -> Float -> Float -> IO ()

foreign import javascript unsafe "destroyImage"
    destroyImage :: Texture -> IO ()

framesPerSecond = 5

animate :: IO () -> IO ()
animate drawScene = do
    rec
        let tick = do
                drawScene
                --threadDelay (1000000 `div` framesPerSecond)
                requestAnimFrame tick'
        tick' <- syncCallback True False tick
    requestAnimFrame tick'

data SpriteState = SpriteState {
        ssInternals  :: Internals WebGL
    }

-- | Get system time in seconds since the start of the Unix epoch
-- (1 Jan 1970).
getTime :: UTCTime -> IO Double
getTime t0 = do
    t <- getCurrentTime
    return $ realToFrac (t `diffUTCTime` t0)

instance Platform WebGL where
    data Args WebGL = WebGLArgs FilePath
    data Internals WebGL = WebGLInternals {
            inResPath  :: FilePath,
            inCache    :: Cache Rect
        }
    data Sprite WebGL = Sprite {
            spKey   :: Key,
            spRect  :: Rect,  -- Bounding box
            spCache :: Maybe (ReaderT SpriteState IO ()),
            spDraw  :: ReaderT SpriteState IO ()
        }
    data Font WebGL = Font
    data Sound WebGL = Sound
    type Touch WebGL = ()

    engine (WebGLArgs resPath) game = do
        canvas <- getElementById "mycanvas"
        width0 <- canvasWidth canvas
        height0 <- canvasHeight canvas
        (viewport, sendViewport) <- sync $ newBehavior (width0, height0)
        let aspect = uncurry (/) <$> viewport

        (time, sendTime) <- sync $ newBehavior 0
        (realTime, sendRealTime) <- sync $ newBehavior 0
        rng <- newStdGen
        (eMouse, sendMouse) <- sync newEvent
        (bSprite, bMusic, eEffects) <- sync $ game aspect eMouse time rng
        spriteRef <- newIORef =<< (Just <$> sync (sample bSprite))
        kill <- sync $ listen (updates bSprite) (writeIORef spriteRef . Just)

        let scaleClick (xx, yy) = do
                (width, height) <- sync $ sample viewport
                let xscale  = 2000 * width / height
                    x = xscale * (xx / width - 0.5)
                    y = (-2000) * (yy / height - 0.5)
                return (x, y)

        md <- syncCallback2 True False $ \jx jy -> do
            Just xx <- fromJSRef jx :: IO (Maybe Float)
            Just yy <- fromJSRef jy :: IO (Maybe Float)
            pt <- scaleClick (xx, yy)
            sync $ sendMouse $ MouseDown () pt
        mu <- syncCallback2 True False $ \jx jy -> do
            Just xx <- fromJSRef jx :: IO (Maybe Float)
            Just yy <- fromJSRef jy :: IO (Maybe Float)
            pt <- scaleClick (xx, yy)
            sync $ sendMouse $ MouseUp () pt
        mm <- syncCallback2 True False $ \jx jy -> do
            Just xx <- fromJSRef jx :: IO (Maybe Float)
            Just yy <- fromJSRef jy :: IO (Maybe Float)
            pt <- scaleClick (xx, yy)
            sync $ sendMouse $ MouseMove () pt

        or <- syncCallback True False $ do
            w <- canvasWidth canvas
            h <- canvasHeight canvas
            resizeViewport canvas w h
            sync $ sendViewport (w,h)
        onWindowResize or

        gl <- initGL canvas md mu mm
        resizeViewport canvas width0 height0

        t0 <- getCurrentTime
        tLastEndRef <- newIORef =<< getTime t0
        timeLostRef <- newIORef 0
        tLastGC <- newIORef 0

        cache <- newCache
        let internals = WebGLInternals {
                    inResPath  = resPath,
                    inCache    = cache
                }

        animate $ do

            t <- readIORef tLastEndRef
            tStart <- getTime t0
            lost <- readIORef timeLostRef
            iHeight <- sync $ do
                sendTime (t - lost)
                sendRealTime t
                round . snd <$> sample viewport
            mSprite <- readIORef spriteRef
            case mSprite of
                Just sprite -> preRunSprite internals iHeight sprite
                Nothing     -> return ()
            tEnd <- getTime t0
            let lost = tEnd - t
            when (lost >= 0.1) $ do
                since <- (\last -> tEnd - last) <$> readIORef tLastGC
                if lost >= 0.25 && since >= 3 then do
                    tEnd' <- getTime t0
                    writeIORef tLastGC tEnd'
                    let lost' = tEnd' - t
                    modifyIORef timeLostRef (+lost')
                  else
                    modifyIORef timeLostRef (+lost)

            case mSprite of
                Just sprite -> do clearColor gl 0 0 0 1
                                  clear gl
                                  runSprite internals iHeight sprite True

                                  writeIORef spriteRef Nothing
                Nothing     -> return ()
            tFinal <- getTime t0
            writeIORef tLastEndRef tFinal
            {-
            putStrLn $ showFFloat (Just 3) (tStart -t) "" ++ " " ++
                       showFFloat (Just 3) (tEnd - tStart) "" ++ " " ++
                       showFFloat (Just 3) (tFinal - tEnd) ""
            -}
            return ()

        -- Keep callbacks alive
        threadDelay maxBound
        kill
        freeCallback md
        freeCallback mu
        freeCallback mm
        freeCallback or

    nullDrawable rect = Sprite NullKey rect Nothing (return ())

    image path = do
        let key = ByteStringKey $ C.pack $ takeFileName path
        return $ \rect ->
            let cacheIt = Just $ do
                    resPath <- asks (inResPath . ssInternals)
                    cache <- asks (inCache . ssInternals)
                    liftIO $ writeCache cache key $ do
                        --putStrLn $ "loading "++path
                        tex <- loadImage $ fromString $ resPath </> path
                        let draw' ((x,y),(w,h)) = drawImage tex x y w h
                            cleanup' = destroyImage tex
                        return (draw', cleanup')
                drawIt = do
                    cache <- asks (inCache . ssInternals)
                    liftIO $ do
                        mDraw <- readCache cache key
                        case mDraw of
                            Just draw -> draw rect
                            Nothing   -> return ()
            in  Sprite key rect cacheIt drawIt

    sound file = return Sound

    retainSound _ _ = return ()

    translateSprite v@(vx, vy) (Sprite key rect cache action) = error "WebGL.translateSprite undefined"
    createFont resPath ycorr = error "WebGL.createFont undefined"
    --uncachedLabel rect@((posX, posY), _) (Color4 r g b _) text = error "WebGL.uncachedLabel undefined"
    key k s = s { spKey = k }
    keyOf d = spKey $ d ((0,0),(0,0))
    setBoundingBox r s = s { spRect = r }
    cache toMultisample (Sprite key rect@(pos, _) cache1 action) = error "WebGL.cache undefined"
    fade brightness (Sprite key rect cache action) = error "WebGL.fade undefined"
    shrink factor (Sprite key rect@((posX,posY),_) cache action) = error "WebGL.shrink undefined"
    preRunSprite internals brightness (Sprite _ _ mCache action) = do
        let ss = SpriteState {
                    ssInternals = internals
                }
        case mCache of
            Just cache -> runReaderT cache ss
            Nothing    -> return ()
    runSprite internals brightness (Sprite _ _ mCache action) flip = do
        let ss = SpriteState {
                    ssInternals = internals
                }
        runReaderT action ss
        when flip $ flipCache (inCache internals)

    audioThread bSounds = error "WebGL.audioThread undefined"
    clockwiseSprite (Sprite key rect cache action) = error "WebGL.clockwiseSprite undefined"
    anticlockwiseSprite (Sprite key rect cache action) = error "WebGL.anticlockwiseSprite undefined"
    rotateSprite theta (Sprite key rect@((posX, posY), _) cache action) = error "WebGL.rotateSprite undefined"

    -- Make this sprite invisible, but allow it to cache anything in the invisible sprite
    invisible (Sprite key rect cache _) = Sprite key rect cache (return ())

    launchURL _ url = error "WebGL.launchURL undefined"
    getSystemLanguage _ = return "en"
