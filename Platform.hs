{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, FlexibleInstances,
        MultiParamTypeClasses, FlexibleContexts #-}
module Platform where

import Geometry

import Data.ByteString.Char8 (ByteString)
import Data.Monoid
import Data.Sequence (Seq)
import Data.Text (Text)
import Graphics.Rendering.OpenGL.GL as GL hiding (Rect)
import Sound.OpenAL
import System.FilePath
import FRP.Sodium
import Text.XML.Expat.Pickle

data TouchPhase = TouchBegan | TouchMoved | TouchEnded | TouchCancelled deriving (Eq, Ord, Show, Enum)

type Touched p = Touch p -> TouchPhase -> Float -> Float -> IO ()

type Drawable p = Rect -> Sprite p

data Key = NullKey
         | BoolKey Bool
         | ByteStringKey ByteString
         | TextKey Text
         | CompositeKey Key Key
         deriving (Eq, Ord, Read, Show)

instance XmlPickler Text Key where
    xpickle = xpPrim

class (Monoid (Sprite p),
       Eq (AssetRef p),
       Show (AssetRef p),
       XmlPickler [UNode Text] (AssetRef p),
       Eq (Touch p),
       Ord (Touch p)) => Platform p where
    data Args p
    data Internals p
    data Sprite p
    data Font p
    data Sound p
    type Touch p
    data AssetRef p
    runGraphics :: Args p -> (Int -> Int -> FilePath -> FilePath -> Internals p -> IO (IO (), IO (), Touched p)) -> IO ()
    nullDrawable :: Drawable p
    nullDrawable = mkDrawable $ \_ -> return ()
    mkDrawable :: (GLfloat -> IO ()) -> Drawable p
    image :: FilePath -> FilePath -> IO (Drawable p)
    sound :: FilePath -> FilePath -> IO (Sound p)
    retainSound :: Sound p -> IO ()
    translateSprite :: (Coord, Coord) -> Sprite p -> Sprite p
    at :: Drawable p -> Rect -> Sprite p
    at = ($)
    createFont :: FilePath
               -> Float         -- Y correction upwards fraction of 1.
               -> IO (Font p)
    key :: Key -> Sprite p -> Sprite p
    keyOf :: Drawable p -> Key
    -- Normally the bounding rectangle (used in caching) is caculated from all components.
    -- This allows you to set it explicitly.
    setBoundingBox :: Rect -> Sprite p -> Sprite p
    -- True if it should multisample
    cache :: Bool -> Sprite p -> Sprite p
    uncachedLabel :: Rect -> Color4 GLfloat -> Text -> Sprite p
    fade :: GLfloat -> Sprite p -> Sprite p
    shrink :: GLfloat -> Sprite p -> Sprite p
    preRunSprite :: Font p -> Internals p -> Int -> Sprite p -> IO ()
    runSprite :: Font p -> Internals p -> Int -> Sprite p -> Bool -> IO ()
    audioThread :: Device -> [(Behavior [Sound p], Float)] -> IO ()
    -- Fetch the user's photo album
    fetchAssets :: (Maybe (Seq (Drawable p, AssetRef p)) -> IO ()) -> IO ()
    fetchAsset :: AssetRef p -> (Maybe (Drawable p) -> IO ()) -> IO ()
    -- Rotate the sprite 90 degrees clockwise
    clockwiseSprite :: Sprite p -> Sprite p
    -- Rotate the sprite 90 degrees anti-clockwise
    anticlockwiseSprite :: Sprite p -> Sprite p
    rotateSprite :: GLfloat -> Sprite p -> Sprite p
    -- Make this sprite invisible, but allow it to cache anything in the invisible sprite
    invisible :: Sprite p -> Sprite p
    launchURL :: p -> ByteString -> IO ()
    getSystemLanguage :: p -> IO ByteString

label :: Platform p => Rect -> Color4 GLfloat -> Text -> Sprite p
label r col txt = cache True $ uncachedLabel r col txt

