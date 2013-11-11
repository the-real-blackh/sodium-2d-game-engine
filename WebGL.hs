{-# LANGUAGE ForeignFunctionInterface, TypeFamilies, FlexibleInstances,
        MultiParamTypeClasses, OverloadedStrings, Rank2Types, EmptyDataDecls,
        JavaScriptFFI, RecursiveDo #-}
module WebGL where

import Cache
import Geometry
import Platform

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
    initGL :: Canvas
           -> JSFun (JSRef Float -> JSRef Float -> IO ())
           -> JSFun (JSRef Float -> JSRef Float -> IO ())
           -> JSFun (JSRef Float -> JSRef Float -> IO ())
           -> IO GL

foreign import javascript unsafe "document.documentElement.clientWidth"
    clientWidth :: IO Float

foreign import javascript unsafe "document.documentElement.clientHeight"
    clientHeight :: IO Float

data Canvas_
type Canvas = JSRef Canvas_

foreign import javascript unsafe "document.getElementById($1)"
    getElementById :: JSString -> IO Canvas

foreign import javascript unsafe "$1.clearColor($2, $3, $4, $5);"
    clearColor :: GL -> Float -> Float -> Float -> Float -> IO ()

foreign import javascript unsafe "$1.viewport(0, 0, $1.viewportWidth, $1.viewportHeight);"
    initViewPort :: GL -> IO ()

foreign import javascript unsafe "$1.clear($1.COLOR_BUFFER_BIT | $1.DEPTH_BUFFER_BIT);"
    clear :: GL -> IO ()

foreign import javascript unsafe "requestAnimFrame($1)"
    requestAnimFrame :: JSFun (IO ()) -> IO ()

data Texture_
type Texture = JSRef Texture_

foreign import javascript unsafe "loadImage"
    loadImage :: JSString -> IO Texture

foreign import javascript unsafe "drawImage"
    drawImage :: Texture -> Float -> Float -> Float -> Float -> IO ()

foreign import javascript unsafe "destroyImage"
    destroyImage :: Texture -> IO ()

animate :: IO () -> IO ()
animate drawScene = do
    rec
        let tick = do
                requestAnimFrame tick'
                drawScene
        tick' <- syncCallback True False tick
    tick

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
    data Args WebGL = WebGLArgs
    data Internals WebGL = WebGLInternals {
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

    engine _ game = do
        canvas <- getElementById "mycanvas"

        width <- clientWidth
        height <- clientHeight
        let xscale  = 2000 * width / height
            iHeight = round height

        (eMouse, sendMouse) <- sync newEvent

        md <- asyncCallback2 True $ \jx jy -> do
            Just xx <- fromJSRef jx :: IO (Maybe Float)
            Just yy <- fromJSRef jy :: IO (Maybe Float)
            let x = xscale * (xx / width - 0.5)
                y = (-2000) * (yy / height - 0.5)
            sync $ sendMouse $ MouseDown () (x,y)
        mu <- asyncCallback2 True $ \jx jy -> do
            Just xx <- fromJSRef jx :: IO (Maybe Float)
            Just yy <- fromJSRef jy :: IO (Maybe Float)
            let x = xscale * (xx / width - 0.5)
                y = (-2000) * (yy / height - 0.5)
            sync $ sendMouse $ MouseUp () (x,y)
        mm <- asyncCallback2 True $ \jx jy -> do
            Just xx <- fromJSRef jx :: IO (Maybe Float)
            Just yy <- fromJSRef jy :: IO (Maybe Float)
            let x = xscale * (xx / width - 0.5)
                y = (-2000) * (yy / height - 0.5)
            sync $ sendMouse $ MouseMove () (x,y)
        gl <- initGL canvas md mu mm
        initViewPort gl

        t0 <- getCurrentTime
        tLastEndRef <- newIORef =<< getTime t0
        timeLostRef <- newIORef 0
        tLastGC <- newIORef 0

        (time, sendTime) <- sync $ newBehavior 0
        (realTime, sendRealTime) <- sync $ newBehavior 0
        rng <- newStdGen
        (bSprite, bMusic, eEffects) <- sync $ game eMouse time rng
        spriteRef <- newIORef =<< sync (sample bSprite)
        kill <- sync $ listen (updates bSprite) (writeIORef spriteRef)

        cache <- newCache
        let internals = WebGLInternals {
                    inCache    = cache
                }

        -- Keep callbacks alive
        forkIO $ do
            threadDelay maxBound
            kill
            freeCallback md
            freeCallback mu
            freeCallback mm

        animate $ do

            t <- readIORef tLastEndRef
            lost <- readIORef timeLostRef
            sync $ do
                sendTime (t - lost)
                sendRealTime t
            sprite <- readIORef spriteRef
            preRunSprite internals iHeight sprite
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

            clearColor gl 0 0 0 1
            clear gl
            sprite <- readIORef spriteRef
            runSprite internals iHeight sprite True
            tEnd <- getTime t0
            writeIORef tLastEndRef tEnd
            _ <- evaluate kill
            return ()

    nullDrawable rect = Sprite NullKey rect Nothing (return ())

    image resDir path = do
        let key = ByteStringKey $ C.pack $ takeFileName path
        return $ \rect ->
            let cacheIt = Just $ do
                    cache <- asks (inCache . ssInternals)
                    liftIO $ writeCache cache key $ do
                        --putStrLn $ "loading "++path
                        tex <- loadImage $ fromString $ resDir </> path
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

    sound resDir file = error "WebGL.sound undefined"

    retainSound Sound = error "WebGL.retainSound undefined"

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
    invisible (Sprite key rect cache _) = error "WebGL.invisible undefined"
    launchURL _ url = error "WebGL.launchURL undefined"
    getSystemLanguage _ = return "en"

