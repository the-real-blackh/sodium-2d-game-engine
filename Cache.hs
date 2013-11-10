module Cache (
        Cache,
        readCache,
        writeCache,
        newCache,
        flipCache
    ) where

import Geometry
import Platform (Key)

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Graphics.Rendering.OpenGL as GL hiding (Triangle, Rect, translate, normal)


data Cache = Cache {
        ccTableRef :: IORef (Map Key Entry)
    }

data Entry = Entry {
        eDraw    :: Point -> GLfloat -> IO (),
        eCleanup :: IO (),
        eTouched :: Bool
    }

newCache :: IO Cache
newCache = Cache <$> newIORef M.empty

readCache :: Cache -> Key -> IO (Maybe (Point -> GLfloat -> IO ()))
readCache cache key = do
    table <- readIORef (ccTableRef cache)
    return $ eDraw `fmap` M.lookup key table

writeCache :: Cache -> Key -> IO (Point -> GLfloat -> IO (), IO ()) -> IO ()
writeCache cache key mkDraw = do
    table <- readIORef (ccTableRef cache)
    case M.lookup key table of
        Just entry -> do
            writeIORef (ccTableRef cache) $ M.insert key (entry { eTouched = True }) table
        Nothing -> do
            (draw, cleanup) <- mkDraw
            let entry = Entry {
                        eDraw    = draw,
                        eCleanup = cleanup,
                        eTouched = True
                    }
            writeIORef (ccTableRef cache) $ M.insert key entry table

flipCache :: Cache -> IO ()
flipCache cache = do
    table <- readIORef (ccTableRef cache)
    let (table', toClean) = M.partition eTouched table
    writeIORef (ccTableRef cache) $ M.map unTouch table'
    --when (not $ M.null toClean) $ print (M.keys toClean)
    case M.elems toClean of
        [] -> return ()
        items -> do
            --putStrLn $ "clean "++show (length items)
            forM_ items $ \e -> eCleanup e
  where
    unTouch e = e { eTouched = False }

