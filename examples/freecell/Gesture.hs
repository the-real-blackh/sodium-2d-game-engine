{-# LANGUAGE RecursiveDo #-}
module Gesture where

import FRP.Sodium
import FRP.Sodium.GameEngine2D.Platform
import FRP.Sodium.GameEngine2D.Geometry
import Control.Applicative


doubleClickTimeout :: Double
doubleClickTimeout = 0.5

doubleClickDistance :: Coord
doubleClickDistance = 10

detectDoubleClick :: Platform p =>
                     Event (MouseEvent p) -> Behavior Double -> Reactive (Event Point, Event (MouseEvent p))
detectDoubleClick eMouse time = do
    rec
        mState <- hold (Nothing) eFirstClk
        let eTriple = snapshotWith (\me (mState, t) ->
                case (me, mState) of
                    (MouseDown _ pt, Nothing) -> (Nothing, [me], Just (pt, t))
                    (_, Just (_, t0)) | t - t0 >= doubleClickTimeout -> (Nothing, [me], Nothing)
                    (MouseMove _ pt, Just (pt0, _)) ->
                        if distance pt pt0 >= doubleClickDistance
                            then (Nothing, [me], Nothing)
                            else (Nothing, [], mState)
                    (MouseUp to _,  Just (pt, t0)) -> (Nothing, [MouseUp to pt], Just (pt, t0))
                    (MouseDown _ _, Just (pt, _)) -> (Just pt, [], Nothing)
                    (_, _) -> (Nothing, [me], mState)
              ) eMouse (liftA2 (,) mState time)
            eDblClick = filterJust $ (\(a, _, _) -> a) <$> eTriple
            eMouse'   = split      $ (\(_, b, _) -> b) <$> eTriple
            eFirstClk = (\(_, _, c) -> c) <$> eTriple
    return (eDblClick, eMouse')
  where
    notMouseDown (MouseDown _ _) = False
    notMouseDown _               = True