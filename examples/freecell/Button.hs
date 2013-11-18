{-# LANGUAGE RecursiveDo #-}
module Button where

import FRP.Sodium.GameEngine2D.Platform
import FRP.Sodium.GameEngine2D.Game (gateMouse)
import FRP.Sodium.GameEngine2D.Geometry
import FRP.Sodium
import Data.Maybe
import Control.Applicative
import Control.Arrow


data Enablement = Enabled | Disabled deriving (Eq, Show)

generalButton :: Platform p =>
                 Behavior Enablement
              -> Behavior Bool        -- ^ Whether to draw it selected
              -> Behavior Rect
              -> (Behavior (Rect, Enablement, Bool, Bool) -> sprite)  -- ^ (enabled, selected,
                                     -- held down) For plain buttons, selected == held down.
                                     -- For toggle buttons such as checkbox, selected is the
                                     -- checked state.
              -> Event (MouseEvent p)
              -> Reactive (sprite, Event Bool, Event (), Behavior Bool)
generalButton enablement selected rect draw eMouse0 = do
    rec
        heldDown <- hold Nothing eHeldDown
        let pair = liftA2 (,) heldDown rect
            eHeldDown = filterJust $ snapshotWith (\mev (heldDown, rect) -> case mev of
                   MouseDown t pos | pos `inside` rect  -> Just (Just t)
                   MouseUp t _     | heldDown == Just t -> Just Nothing
                   _                                    -> Nothing
               ) eMouse pair
    let boolHeldDown = isJust <$> heldDown
        sprite = draw $ (,,,) <$> rect <*> enablement <*> selected <*> boolHeldDown
        eClicked = filterJust $ snapshotWith (\mev (heldDown, rect) -> case mev of
            MouseDown _ pos | pos `inside` rect  -> Just True
            MouseUp t _     | heldDown == Just t -> Just False  -- must be the same touch as the down
            _                                    -> Nothing) eMouse pair
        eSound = filterJust $ (\down -> if down then Just () else Nothing) <$> eClicked
    return (sprite, eClicked, eSound, boolHeldDown)
  where
    -- Ignore input events if this button is disabled.
    eMouse = eMouse0 `gateMouse` ((== Enabled) <$> enablement)

-- | Returns (sprite to draw, button selected (clicked), button sound)
plainButton :: Platform p =>
               Behavior Enablement
            -> Behavior Rect
            -> (Behavior (Rect, Enablement, Bool) -> sprite)
            -> Event (MouseEvent p)
            -> Reactive (sprite, Event (), Event ())
plainButton enablement rect draw eMouse = do
    rec
        (sprite, eClicked, eSound, heldDown) <- generalButton enablement heldDown rect draw' eMouse
    let eSel = filterJust $ (\down -> if down then Nothing else Just ()) <$> eClicked
    return (sprite, eSel, eSound)
  where
    draw' b = draw $ fmap (\(rect, ena, _, heldDown) -> (rect, ena, heldDown)) b

toggleButton :: Platform p =>
                Behavior Enablement
             -> Behavior Bool  -- ^ Selected state
             -> Behavior Rect
             -> (Behavior (Rect, Enablement, Bool, Bool) -> sprite)
             -> Event (MouseEvent p)
             -> Reactive (sprite, Event Bool, Event ())
toggleButton enablement state rect draw eMouse = do
    rec
        let eSel = snapshotWith (\down selected -> if down then not selected else selected)
                        eClicked state
        (sprite, eClicked, eSound, _) <- generalButton enablement state rect draw eMouse
    return (sprite, eSel, eSound)

