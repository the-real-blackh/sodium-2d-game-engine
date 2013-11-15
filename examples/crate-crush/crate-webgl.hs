import CrateCrush
import FRP.Sodium.GameEngine2D.Platform (engine)
import FRP.Sodium.GameEngine2D.WebGL


main = do
    let args = WebGLArgs "resources"
    game <- crateCrush args
    engine args game
