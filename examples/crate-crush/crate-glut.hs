import CrateCrush
import FRP.Sodium.GameEngine2D.GLUT
import FRP.Sodium.GameEngine2D.Platform (engine)


main = do
    let args = GLUTArgs "Crate Crush" "."
    game <- crateCrush args
    engine args game
