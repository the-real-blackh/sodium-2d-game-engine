{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
module FRP.Sodium.GameEngine2D.Game where

import FRP.Sodium.GameEngine2D.Geometry
import FRP.Sodium.GameEngine2D.Platform

import FRP.Sodium
import System.Random (StdGen)


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

