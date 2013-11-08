import Freecell
import Platform (engine)
import WebGL


main = do
    game <- freecell
    engine WebGLArgs game
