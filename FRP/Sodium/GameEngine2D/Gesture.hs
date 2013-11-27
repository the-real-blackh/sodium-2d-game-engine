{-# LANGUAGE RecursiveDo #-}
module FRP.Sodium.GameEngine2D.Gesture where

import Data.Monoid
import FRP.Sodium
import FRP.Sodium.GameEngine2D.Platform
import FRP.Sodium.GameEngine2D.Geometry
import Control.Applicative


doubleClickTimeout :: Double
doubleClickTimeout = 0.5

-- | If you move more than this distance during a click, then it's a drag.
maxClickDistance :: Coord
maxClickDistance = 10

-- | A click is a MouseDown followed by a MouseUp where the distance travelled
-- was less than maxClickDistance.
clickGesture :: Platform p =>
                Event (MouseEvent p)
             -> Reactive (Event Point)
clickGesture eMouse = do
    mState <- accum Nothing $ flip fmap eMouse $ \me state ->
            case (me, state) of
                (MouseDown t pt, _)                      -> Just (t, pt)
                (MouseUp   t pt, Just (t0, _)) | t == t0 -> Nothing
                _                                        -> state
    return $ filterJust $ snapshotWith (\me state -> case (me, state) of
            (MouseUp t pt, Just (t0, pt0)) | t == t0 && distance pt pt0 < maxClickDistance -> Just pt
            _                                                           -> Nothing
        ) eMouse mState

-- | Returns the double click event and the filtered mouse event, where the
-- second click is taken out.
doubleClickGesture :: Platform p =>
                      Event (MouseEvent p)
                   -> Behavior Double
                   -> Reactive (Event Point, Event (MouseEvent p))
doubleClickGesture eMouse time = do
    rec
        mState <- hold Nothing eFirstClk
        let eTriple = snapshotWith (\me (mState, t) ->
                case (me, mState) of
                    (MouseDown _ pt, Nothing) -> (Nothing, [me], Just (pt, t))
                    (_, Just (_, t0)) | t - t0 >= doubleClickTimeout -> (Nothing, [me], Nothing)
                    (MouseMove _ pt, Just (pt0, _)) ->
                        if distance pt pt0 >= maxClickDistance
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

