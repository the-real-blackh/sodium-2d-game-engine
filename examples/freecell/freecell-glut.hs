import Freecell
import FRP.Sodium.GameEngine2D.GLUT
import FRP.Sodium.GameEngine2D.Platform (engine)


main = do
    game <- freecell "template/cards"
    engine (GLUTArgs "freecell") game
