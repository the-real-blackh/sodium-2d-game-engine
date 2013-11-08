{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
module Game where

import Geometry
import Platform
import FRP.Sodium
import System.Random (StdGen)


type Game p = Event (MouseEvent p)
           -> Behaviour Double
           -> StdGen
           -> Reactive (Behaviour (Sprite p))

data MouseEvent p = MouseDown (Touch p) Point | MouseMove (Touch p) Point | MouseUp (Touch p) Point

deriving instance Show (Touch p) => Show (MouseEvent p)

-- | Like 'gate' except it only blocks mouse down events, otherwise we get weird
-- effects.
gateMouse :: Event (MouseEvent p) -> Behavior Bool -> Event (MouseEvent p)
gateMouse e b = filterJust $ snapshotWith fmouse e b
  where
    fmouse (MouseDown _ _) False = Nothing
    fmouse m _                   = Just m

-- | Rotate the mouse point 90 degrees clockwise
clockwiseMouse :: MouseEvent p -> MouseEvent p
clockwiseMouse (MouseDown t pt) = MouseDown t (clockwisePoint pt)
clockwiseMouse (MouseMove t pt) = MouseMove t (clockwisePoint pt)
clockwiseMouse (MouseUp t pt)   = MouseUp t (clockwisePoint pt)

-- | Rotate the mouse point 90 degrees anti-clockwise
anticlockwiseMouse :: MouseEvent p -> MouseEvent p
anticlockwiseMouse (MouseDown t pt) = MouseDown t (anticlockwisePoint pt)
anticlockwiseMouse (MouseMove t pt) = MouseMove t (anticlockwisePoint pt)
anticlockwiseMouse (MouseUp t pt)   = MouseUp t (anticlockwisePoint pt)

