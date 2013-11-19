{-# LANGUAGE JavaScriptFFI #-}
import Freecell

import Control.Monad
import FRP.Sodium.GameEngine2D.Platform (engine)
import FRP.Sodium.GameEngine2D.WebGL
import Data.Text (Text)
import qualified Data.Text as T
import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Marshal


foreign import javascript unsafe "backgrounds.length"
    getNoOfBackgrounds :: IO Int

foreign import javascript unsafe "backgrounds[$1].file"
    getFile :: Int -> IO (JSRef Text)

foreign import javascript unsafe "backgrounds[$1].duration"
    getDuration :: Int -> IO Double

main = do
    no <- getNoOfBackgrounds
    bgFiles <- forM [0..no-1] $ \i -> do
        jf <- getFile i
        Just f <- fromJSRef jf :: IO (Maybe Text)
        d <- getDuration i
        return $ Background (T.unpack f) d
    game <- freecell bgFiles
    engine (WebGLArgs "resources") game
