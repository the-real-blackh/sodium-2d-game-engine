import Freecell
import FRP.Sodium.GameEngine2D.GLUT
import FRP.Sodium.GameEngine2D.Platform (engine)


main = do
    game <- freecell
    engine (GLUTArgs "freecell" "resources") game
