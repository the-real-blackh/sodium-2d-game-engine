import Engine
import Freecell
import WebGL


main = do
    game <- freecell
    mainProgram WebGLArgs game
