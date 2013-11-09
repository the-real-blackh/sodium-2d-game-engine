{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, FlexibleInstances,
        MultiParamTypeClasses, FlexibleContexts, StandaloneDeriving,
        UndecidableInstances #-}
module Platform where

import Geometry

import Data.ByteString.Char8 (ByteString)
import Data.Monoid
import Data.Sequence (Seq)
import Data.Text (Text)
import FRP.Sodium
import Graphics.Rendering.OpenGL.GL as GL hiding (Rect)
import Sound.OpenAL
import System.FilePath
import System.Random (StdGen)
import Text.XML.Expat.Pickle

data MouseEvent p = MouseDown (Touch p) Point | MouseMove (Touch p) Point | MouseUp (Touch p) Point

deriving instance Show (Touch p) => Show (MouseEvent p)

type Game p = Event (MouseEvent p)
           -> Behaviour Double
           -> StdGen
           -> Reactive (Behaviour (Sprite p))

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
       Eq (Touch p),
       Ord (Touch p)) => Platform p where
    data Args p
    data Internals p
    data Sprite p
    data Font p
    data Sound p
    type Touch p
    engine :: Args p -> Game p -> IO ()
    nullDrawable :: Drawable p
    nullDrawable = mkDrawable $ \_ -> return ()
    mkDrawable :: (GLfloat -> IO ()) -> Drawable p
    image :: FilePath -> FilePath -> IO (Drawable p)
    sound :: FilePath -> FilePath -> IO (Sound p)
    retainSound :: Sound p -> IO ()
    translateSprite :: (Coord, Coord) -> Sprite p -> Sprite p
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
    preRunSprite :: Internals p -> Int -> Sprite p -> IO ()
    runSprite :: Internals p -> Int -> Sprite p -> Bool -> IO ()
    audioThread :: Device -> [(Behavior [Sound p], Float)] -> IO ()
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

