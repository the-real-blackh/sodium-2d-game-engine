import Engine
import Freecell
import GLUT


main = do
    game <- freecell
    mainProgram (GLUTArgs "freecell") game
