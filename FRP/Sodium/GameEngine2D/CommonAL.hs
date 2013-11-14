{-# LANGUAGE ScopedTypeVariables, RankNTypes, BangPatterns #-}
module FRP.Sodium.GameEngine2D.CommonAL where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Maybe
import FRP.Sodium
import Sound.OpenAL
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Codec.Audio.Vorbis.File as V
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable


data SoundInfo = SoundPath FilePath
               | SoundImage V.Info [ByteString]

spool :: SoundInfo -> (V.Info -> IO (Maybe ByteString) -> IO a) -> IO a
spool (SoundImage info initBlocks) code = do
    blocksRef <- newIORef initBlocks
    code info $ do
        blocks <- readIORef blocksRef
        case blocks of
            [] -> return Nothing
            (blk:blks) -> do
                writeIORef blocksRef blks
                return $ Just blk
spool (SoundPath path) code = do
    remRef <- newIORef $ Just B.empty
    V.withFile path $ \f -> do
        info <- V.info f
        let factor = case V.inChannels info of
                V.Mono   -> 1
                V.Stereo -> 2
            blkSize = 22050 * factor   -- 1/4 of a second @ 44100 kHZ
            loop chunks !bytes = do
                mChunk <- V.read f blkSize V.getSystemEndianness V.SixteenBit V.Signed
                case mChunk of
                    Just (chunk, _) -> do
                        let chunks' = chunk:chunks
                        if bytes < blkSize then
                            loop chunks' (bytes + B.length chunk)
                          else do
                            let (block, rem) = B.splitAt blkSize . B.concat . reverse $ chunks'
                            writeIORef remRef $ Just rem
                            return $ Just block
                    Nothing -> do
                        writeIORef remRef Nothing
                        let block = B.concat . reverse $ chunks
                        if B.null block
                            then return Nothing
                            else return $ Just block
        code info $ do
            mRem <- readIORef remRef
            case mRem of
                Just rem -> loop [rem] (B.length rem)
                Nothing  -> return Nothing

loadSound :: SoundInfo -> IO SoundInfo
loadSound si = spool si $ \info getBlock ->
    SoundImage info <$> unfoldM getBlock

unfoldM :: Monad m => m (Maybe a) -> m [a]
unfoldM fetch = go []
  where
    go acc = do
        ma <- fetch
        case ma of
            Just a -> go (a:acc)
            Nothing -> return $ reverse acc

alOpenDevice :: IO (Maybe Device)
alOpenDevice = openDevice Nothing

alAudioThread :: Device -> [(Behavior [IORef SoundInfo], ALfloat)] -> IO ()
alAudioThread device bSounds = do
    Just context <- createContext device []
    currentContext $= Just context

    forM_ bSounds $ \(bSound, gain) -> forkIO $ do
        tv <- atomically $ newTVar Nothing
        kill <- sync $ do
            let v = coalesce const (value bSound)
            listen v $ \fRefs -> atomically $ writeTVar tv (Just fRefs)

        (src, buffers) <- do
            [src] <- genObjectNames 1
            sourceGain src $= gain

            buffers <- genObjectNames 8   -- 1/4 of a sec each, so that's 2 seconds worth
            return (src, buffers)

        printErrors
        let startPlaying = do
                playing <- (== Playing) <$> get (sourceState src)
                when (not playing) $ play [src]        -- set status to Playing

        forever $ do
            fs <- atomically $ do
                mFS <- readTVar tv
                case mFS of
                    Just fs -> do
                        writeTVar tv Nothing
                        return fs
                    Nothing -> retry
            freeRef <- newIORef buffers
            let playIt _   []         = return ()
                playIt fs0 (soundRef:rem) = do
                    sound <- readIORef soundRef
                    continue <- spool sound $ \info getBlock -> do
                        let fmt = case V.inChannels info of
                                V.Mono   -> Mono16
                                V.Stereo -> Stereo16
                            blocksToQueueInitially = 3
                            -- use allocated buffers till they're all gone, then start popping
                            -- them off the end of the audio queue as OpenAL finishes with them.
                            getBuffer = do
                                free <- readIORef freeRef
                                case free of
                                    (b:free') -> do
                                        writeIORef freeRef free'
                                        return b
                                    _ -> do
                                        completed <- fromIntegral <$> get (buffersProcessed src)
                                        if completed == 0 then do
                                            threadDelay 100000
                                            getBuffer
                                          else do
                                            head <$> unqueueBuffers src 1
                            queueBlock blk = do
                                b <- getBuffer
                                B.unsafeUseAsCStringLen blk $ \(ptr, len) -> do
                                    bufferData b $= BufferData (MemoryRegion ptr (fromIntegral len))
                                        fmt (fromIntegral $ V.inRate info)
                                    printErrors
                                    queueBuffers src [b]
                            loop !blocksQueued = do
                                mBLK <- getBlock
                                case mBLK of
                                    Just blk -> do
                                        queueBlock blk
                                        when (blocksQueued == blocksToQueueInitially) $ do
                                            startPlaying
                                        fsDesired <- atomically $ readTVar tv
                                        if isNothing fsDesired
                                            then loop (blocksQueued+1)
                                            else return False  -- Exit if a new instruction comes through
                                    Nothing -> do
                                        -- play any remaining data
                                        when (blocksQueued <= blocksToQueueInitially)
                                            startPlaying
                                        return True
                        loop 0
                    when continue $ playIt fs0 rem
            stop [src]
            buffer src $= Nothing
            playIt fs fs
        kill

-- | Print all OpenAL errors if applicable
printErrors :: IO ()
printErrors = do
    e <- get alErrors
    when (not $ null e) $ print e

