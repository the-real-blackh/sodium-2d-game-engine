{-# LANGUAGE ForeignFunctionInterface, TypeFamilies, FlexibleInstances,
        MultiParamTypeClasses, OverloadedStrings, Rank2Types, EmptyDataDecls #-}
module WebGL where

import Geometry
import Platform

import Control.Applicative
import Data.Time.Clock
import System.Random (newStdGen)


-- | Get system time in seconds since the start of the Unix epoch
-- (1 Jan 1970).
getTime :: UTCTime -> IO Double
getTime t0 = do
    t <- getCurrentTime
    return $ realToFrac (t `diffUTCTime` t0)

data WebGL

instance Monoid (Sprite WebGL) where
    mempty = WebGL $ return ()
    Sprite ma `mappend` Sprite mb = Sprite (ma >> mb)

instance Platform WebGL where
    data Args WebGL = WebGLArgs
    data Internals WebGL = WebGLInternals
    data Sprite WebGL = Sprite (IO ())
    data Font WebGL = Font {
            ftFont :: FTGL.Font,
            ftYCorr :: Float
        }
    newtype Sound WebGL = Sound (IORef SoundInfo)
    type Touch WebGL = ()

    engine _ game = do

    image resDir path = do
        return $ \rect@((posX, posY),(sizeX, sizeY)) ->

    sound resDir file = error "WebGL.sound undefined"

    retainSound (Sound siRef) = error "WebGL.retainSound undefined"

    translateSprite v@(vx, vy) (Sprite key rect cache action) = error "WebGL.translateSprite undefined"
    createFont resPath ycorr = error "WebGL.createFont undefined"
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

