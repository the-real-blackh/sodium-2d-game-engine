import Freecell
import FRP.Sodium.GameEngine2D.GLUT
import FRP.Sodium.GameEngine2D.Platform (engine)


main = do
    game <- freecell [
            Background "background.jpg" 1.98 10 {-,
            Background "s1.png" 1.98 10 -}
        ]
    engine (GLUTArgs "freecell" "resources") game

