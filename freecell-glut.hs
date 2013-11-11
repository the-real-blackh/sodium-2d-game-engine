import Freecell
import GLUT
import Platform (engine)


main = do
    game <- freecell "template/cards"
    engine (GLUTArgs "freecell") game
