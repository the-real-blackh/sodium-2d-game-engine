{-# LANGUAGE RecursiveDo #-}
module FRP.Sodium.GameEngine2D.Gesture (
        clickGesture,
        doubleClickGesture,
        dragGesture
    ) where

import Data.Maybe
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
-- was less than maxClickDistance. The second value is whether the mouse is
-- held down.
clickGesture :: Platform p =>
                Behavior (Point -> Bool)   -- ^ Is inside the object?
             -> Event (MouseEvent p)
             -> Reactive (Event Point, Behavior Bool)
clickGesture inside eMouse = do
    (eClick, _, heldDown) <- identifyPress inside eMouse
    return (eClick, heldDown)

-- | Returns the double click event and the filtered mouse event, where the
-- second click is taken out.
doubleClickGesture :: Platform p =>
                      Behavior (Point -> Bool)   -- ^ Is inside the object?
                   -> Event (MouseEvent p)
                   -> Behavior Double
                   -> Reactive (Event Point, Event (MouseEvent p))
doubleClickGesture inside eMouse time = do
    rec
        mState <- hold Nothing eFirstClk
        let eTriple = snapshot (\me (mState, t, inside) ->
                case (me, mState) of
                    (MouseDown _ pt, Nothing) | inside pt -> (Nothing, [me], Just (pt, t))
                    (_, Just (_, t0)) | t - t0 >= doubleClickTimeout -> (Nothing, [me], Nothing)
                    (MouseMove _ pt, Just (pt0, _)) ->
                        if distance pt pt0 >= maxClickDistance
                            then (Nothing, [me], Nothing)
                            else (Nothing, [], mState)
                    (MouseUp to _,  Just (pt, t0)) -> (Nothing, [MouseUp to pt], Just (pt, t0))
                    (MouseDown _ _, Just (pt, _)) -> (Just pt, [], Nothing)
                    (_, _) -> (Nothing, [me], mState)
              ) eMouse (liftA3 (,,) mState time inside)
            eDblClick = filterJust $ (\(a, _, _) -> a) <$> eTriple
            eMouse'   = split      $ (\(_, b, _) -> b) <$> eTriple
            eFirstClk = (\(_, _, c) -> c) <$> eTriple
    return (eDblClick, eMouse')
  where
    notMouseDown (MouseDown _ _) = False
    notMouseDown _               = True

-- | A mouse press can go one of two ways. Either it gets released while still
-- inside maxClickDistance, in which case it's a single mouse click, or it is
-- held down and we go outside maxClickDistance, in which case it's the initiation
-- of a drag gesture.
identifyPress :: Platform p =>
                 Behavior (Point -> Bool)   -- ^ Is inside the object?
              -> Event (MouseEvent p)
              -> Reactive (Event Point, Event (Touch p, Point), Behavior Bool)
identifyPress inside eMouse = do
    rec
        pending <- hold Nothing $ eDown <> (const Nothing <$> eUp) <> (const Nothing <$> eDragStart)
        let eDown = filterJust $ snapshot (\me (pending, inside) ->
                    case (me, pending) of
                        (MouseDown t pt, _) | inside pt -> Just $ Just (t, pt)
                        _ -> Nothing
                ) eMouse (liftA2 (,) pending inside)
            eDragStart = filterJust $ snapshot (\me pending ->
                    case (me, pending) of
                        (MouseMove t pt, Just (t0, pt0))
                            | t == t0 && distance pt pt0 >= maxClickDistance -> Just (t0, pt0)
                        _ -> Nothing
                ) eMouse pending
            eUp = filterJust $ snapshot (\me pending ->
                    case (me, pending) of
                        (MouseUp t pt, Just (t0, pt0)) | t == t0 -> Just pt0
                        _ -> Nothing
                ) eMouse pending
    return (eUp, eDragStart, isJust <$> pending)

-- | Returns offset dragged by, and offset dropped to.
dragGesture :: Platform p =>
               Behavior (Point -> Bool)   -- ^ Are we inside the object?
            -> Event (MouseEvent p)
            -> Reactive (Behavior (Maybe Vector), Event Vector)
dragGesture inside eMouse = do
    eStart <- fmap Just . (\(_, es, _) -> es) <$> identifyPress inside eMouse
    rec
        dragging <- hold Nothing (eStart <> eStop)

        let eDrop = filterJust $ snapshot (\me dragging ->
                    case (me, dragging) of
                        (MouseUp t pt, Just (t0, pt0)) -> Just (pt `minus` pt0)
                        _                              -> Nothing
                ) eMouse dragging
            eStop = const Nothing <$> eDrop
    let eDragPos = filterJust $ snapshot (\me dragging ->
                case dragging of
                    Just (t0, pt0) | meTouch me == t0 -> Just $ Just (mePosition me `minus` pt0)
                    _                                 -> Nothing
            ) eMouse dragging
    -- eDrop and eDragPos will be simultaneous events when the mouse button is
    -- released. We give precedence to eDrop.
    dragPos <- hold Nothing $ mergeWith const (const Nothing <$> eDrop) eDragPos 
    return (dragPos, eDrop)

