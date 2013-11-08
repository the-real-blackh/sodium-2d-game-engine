import Control.Monad
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M
import GLUT
import MainProgram
import Freecell
import Platform
import System.FilePath


cardFn :: Card -> FilePath
cardFn (Card v s) = "cards" ++ [pathSeparator] ++ suitName s ++ valueName v ++ ".png"
  where
    suitName Spades = "s"
    suitName Clubs = "c"
    suitName Diamonds = "d"
    suitName Hearts = "h"
    valueName Ace = "1"
    valueName Two = "2"
    valueName Three = "3"
    valueName Four = "4"
    valueName Five = "5"
    valueName Six = "6"
    valueName Seven = "7"
    valueName Eight = "8"
    valueName Nine = "9"
    valueName Ten = "10"
    valueName Jack = "j"
    valueName Queen = "q"
    valueName King = "k"

main = do
    cards <- forM [minBound..maxBound] $ \card -> do
        i <- image "cards" (cardFn card)
        return (card, i)
    let cardsM = M.fromList cards
        draw card = fromJust $ M.lookup card cardsM
    emptySpace <- image "cards" "cards/empty-space.png"
    mainProgram (GLUTArgs "tables game") (freecell draw emptySpace)
