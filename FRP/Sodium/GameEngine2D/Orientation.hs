module FRP.Sodium.GameEngine2D.Orientation where


data Orientation = OrientationUp
                 | OrientationDown
                 | OrientationLeft
                 | OrientationRight
                 | OrientationUpMirrored
                 | OrientationDownMirrored
                 | OrientationLeftMirrored
                 | OrientationRightMirrored
                 deriving (Eq, Ord, Show, Enum)

