module FRP.Sodium.GameEngine2D.CleanMouse where

import FRP.Sodium
import FRP.Sodium.GameEngine2D.Platform
import Data.Set (Set)
import qualified Data.Set as S


-- | On some browsers we get dirty events. If we get two MouseDown events in a row,
-- then we throw out the second one.
cleanMouse :: Platform p =>
              Event (MouseEvent p) -> Reactive (Event (MouseEvent p))
cleanMouse eMouse = do
    downs <- accum S.empty $ fmap (\me -> 
            case me of
                MouseDown to _ -> S.insert to
                MouseUp   to _ -> S.delete to
                _              -> id
           ) eMouse
    return $ filterJust $ snapshot (\me downs ->
            case me of
                -- Suppress MouseDown if this touch is already down.
                MouseDown to _ | to `S.member` downs -> Nothing
                _ ->  Just me
        ) eMouse downs
