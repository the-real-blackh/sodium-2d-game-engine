{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, FlexibleInstances,
        MultiParamTypeClasses, FlexibleContexts, StandaloneDeriving,
        UndecidableInstances #-}
module FRP.Sodium.GameEngine2D.Platform where

import FRP.Sodium.GameEngine2D.Geometry

import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import Data.Default
import Data.Monoid
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as T
import FRP.Sodium
import System.FilePath
import System.Random (StdGen)

data MouseEvent p =
    MouseDown { meTouch :: Touch p,  mePosition :: Point } |
    MouseMove { meTouch :: Touch p,  mePosition :: Point } |
    MouseUp   { meTouch :: Touch p,  mePosition :: Point }

deriving instance Show (Touch p) => Show (MouseEvent p)

mouseTouch :: MouseEvent p -> Touch p
mouseTouch (MouseDown to _) = to
mouseTouch (MouseMove to _) = to
mouseTouch (MouseUp to _) = to

mousePosition :: MouseEvent p -> Point
mousePosition (MouseDown _ pt) = pt
mousePosition (MouseMove _ pt) = pt
mousePosition (MouseUp _ pt) = pt

-- | Like 'gate' except it only blocks mouse down events, otherwise we get weird
-- effects.
gateMouse :: Event (MouseEvent p) -> Behavior Bool -> Event (MouseEvent p)
gateMouse e b = filterJust $ snapshotWith fmouse e b
  where
    fmouse (MouseDown _ _) False = Nothing
    fmouse m _                   = Just m

data GameInput p = GameInput {
        giAspect :: Behavior Coord,
        giMouse  :: Event (MouseEvent p),
        giTime   :: Behavior Double,
        giRNG0   :: StdGen
    }

data GameOutput p = GameOutput {
        goSprite  :: Behavior (Sprite p),
        goMusic   :: Behavior (Text, [Sound p]),
        goEffects :: Event (Sound p) 
    }

instance Platform p => Default (GameOutput p) where
    def = GameOutput (pure mempty) (pure (T.empty, [])) never

data TouchPhase = TouchBegan | TouchMoved | TouchEnded | TouchCancelled deriving (Eq, Ord, Show, Enum)

type Touched p = Touch p -> TouchPhase -> Coord -> Coord -> IO ()

type Drawable p = Rect -> Sprite p

data Key = NullKey
         | BoolKey Bool
         | ByteStringKey ByteString
         | TextKey Text
         | CompositeKey Key Key
         deriving (Eq, Ord, Read, Show)

appendKey :: Key -> Key -> Key
appendKey NullKey k = k
appendKey k NullKey = k
appendKey k1 k2 = CompositeKey k1 k2

engine :: Platform p =>
          Args p -> (GameInput p -> Reactive (GameOutput p)) -> IO ()
engine args game = engine' args $ \gi run -> sync (game gi) >>= run

class (Monoid (Sprite p),
       Eq (Touch p),
       Ord (Touch p)) => Platform p where
    data Args p
    data Internals p
    data Sprite p
    data Font p
    data Sound p
    type Touch p
    engine' :: Args p -> (GameInput p -> (GameOutput p -> IO ()) -> IO ()) -> IO ()
    nullDrawable :: Drawable p
    image :: FilePath -> IO (Drawable p)
    backgroundImage :: FilePath -> IO (Sprite p)
    sound :: FilePath -> IO (Sound p)
    retainSound :: Args p -> Sound p -> IO ()
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
    --uncachedLabel :: Rect -> Color4 GLfloat -> Text -> Sprite p
    fade :: Coord -> Sprite p -> Sprite p
    shrink :: Coord -> Sprite p -> Sprite p
    preRunSprite :: Internals p -> Int -> Sprite p -> IO ()
    runSprite :: Internals p -> Int -> Sprite p -> Bool -> IO ()
    audioThread :: Internals p -> [(Behavior [Sound p], Float)] -> IO ()
    -- Rotate the sprite 90 degrees clockwise
    clockwiseSprite :: Sprite p -> Sprite p
    -- Rotate the sprite 90 degrees anti-clockwise
    anticlockwiseSprite :: Sprite p -> Sprite p
    rotateSprite :: Coord -> Sprite p -> Sprite p
    -- Make this sprite invisible, but allow it to cache anything in the invisible sprite
    invisible :: Sprite p -> Sprite p
    launchURL :: p -> ByteString -> IO ()
    getSystemLanguage :: p -> IO ByteString

{-
label :: Platform p => Rect -> Color4 GLfloat -> Text -> Sprite p
label r col txt = cache True $ uncachedLabel r col txt
-}

