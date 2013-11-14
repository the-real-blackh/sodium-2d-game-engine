import Freecell
import FRP.Sodium.GameEngine2D.Platform (engine)
import FRP.Sodium.GameEngine2D.WebGL


main = do
    game <- freecell "cards"
    engine WebGLArgs game
