import Freecell
import GLUT
import Platform (engine)


main = do
    game <- freecell
    engine (GLUTArgs "freecell") game
