import Freecell
import Platform (engine)
import WebGL


main = do
    game <- freecell "cards"
    engine WebGLArgs game
