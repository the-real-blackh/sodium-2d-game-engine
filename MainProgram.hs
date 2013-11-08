{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module MainProgram where

import Platform
import Game (MouseEvent(..))

import Control.Applicative
import FRP.Sodium
import Foreign.Ptr
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.IORef
import Data.Maybe
import Data.Monoid
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Foreign
import Foreign.C
import Graphics.Rendering.OpenGL.GL as GL
import Numeric
import Sound.OpenAL (openDevice)
import System.Directory
import System.Environment
import System.FilePath
import System.IO.Error
import System.Mem
import System.Random
import Data.Time.Clock
import Text.XML.Expat.Format
import Text.XML.Expat.Tree
import Text.XML.Expat.Pickle

-- | Get system time in seconds since the start of the Unix epoch
-- (1 Jan 1970).
getTime :: UTCTime -> IO Double
getTime t0 = do
    t <- getCurrentTime
    return $ realToFrac (t `diffUTCTime` t0)

mainProgram :: forall p . Platform p =>
               Args p
            -> (Event (MouseEvent p) -> Behaviour Double -> StdGen -> Reactive (Behaviour (Sprite p)))
            -> IO ()
mainProgram r game = runGraphics r $ \width height resourceDir stateDir internals -> do
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
