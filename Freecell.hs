{-# LANGUAGE DoRec #-}
module Freecell (freecell) where

import Game
import Geometry
import FRP.Sodium
import Control.Applicative
import Control.Monad
import Data.Traversable (sequenceA)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Platform
import System.Random
import System.FilePath
import Data.Array.IArray as A
import Data.Array.ST

data Suit  = Spades | Clubs | Diamonds | Hearts
             deriving (Eq, Ord, Show, Enum, Bounded)
data Value = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
             deriving (Eq, Ord, Show, Enum, Bounded)
data Card  = Card Value Suit
             deriving (Eq, Ord, Show)

instance Enum Card where
    fromEnum (Card v s) = fromEnum v + fromEnum s * 13
    toEnum i = Card (toEnum v) (toEnum s)
      where
        (s, v) = divMod i 13

instance Bounded Card where
    minBound = Card minBound minBound
    maxBound = Card maxBound maxBound 

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

freecell :: Platform p => IO (Game p)
freecell = do
    cards <- forM [minBound..maxBound] $ \card -> do
        i <- image "cards" (cardFn card)
        return (card, i)
    let cardsM = M.fromList cards
        draw card = fromJust $ M.lookup card cardsM
    emptySpace <- image "cards" "cards/empty-space.png"
    return $ game draw emptySpace

noOfStacks :: Int
noOfStacks = 8

noOfCells :: Int
noOfCells = 4

cardSize :: Vector
cardSize = (100,150)

overlapY :: Float
overlapY = 70

data Location = Stack Int | Cell Int | Grave deriving (Eq, Show)

data Bunch = Bunch {
        buInitOrig     :: Point,
        buInitMousePos :: Point,
        buCards        :: [Card],
        buOrigin       :: Location
    }
    deriving Show

data Destination = Destination {
        deLocation :: Location,
        deDropZone :: Rect,
        deMayDrop  :: [Card] -> Bool
    }

validSequence :: [Card] -> Bool
validSequence xs = and $ zipWith follows xs (drop 1 xs)

follows :: Card -> Card -> Bool
follows one@(Card v1 _) two@(Card v2 _) = isRed one /= isRed two && (v1 /= Ace && pred v1 == v2)
  where
    isRed :: Card -> Bool
    isRed (Card _ suit) = suit == Hearts || suit == Diamonds

cardSpacing :: Coord
cardSpacing = (2000-cardWidth) / fromIntegral (noOfStacks-1)
  where
    (cardWidth, _) = cardSize

cardSpacingNarrow :: Coord
cardSpacingNarrow = cardSpacing * 0.9

topRow :: Coord
topRow = 1000 - 50 - cardHeight
  where
    (cardWidth, cardHeight) = cardSize

{-
draw :: Point -> Card -> Sprite p
draw pt (Card v s) = ((pt, cardSize), "cards" ++ [pathSeparator] ++ suitName s ++ valueName v ++ ".png")
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
-}

{-
emptySpace :: Point -> Sprite p
emptySpace pt = ((pt, cardSize), "cards" ++ [pathSeparator] ++ "empty-space.png") 
-}

-- | The vertical stacks of cards, where cards can only be added if they're
-- descending numbers and alternating red-black.
stack :: Platform p =>
         (Card -> Drawable p)
      -> Event (MouseEvent p) -> [Card] -> Location -> Behavior Int -> Event [Card]
      -> Reactive (Behavior [Sprite p], Behavior Destination, Event Bunch)
stack draw eMouse initCards loc@(Stack ix) freeSpaces eDrop = do
    let (cardWidth, cardHeight) = cardSize
        orig@(origX, origY) = (
                (-1000) + cardWidth*0.5 + fromIntegral ix * cardSpacing,
                300
            )
        positions = iterate (\(x, y) -> (x, y-overlapY)) orig
    rec
        cards <- hold initCards (eRemoveCards `merge` eAddCards)
        let eAddCards = snapshotWith (\newCards cards -> cards ++ newCards) eDrop cards
            eMouseSelection = filterJust $ snapshotWith (\mev cards ->
                    case mev of
                        MouseDown _ pt@(x, y) | x >= origX - cardWidth && x <= origX + cardWidth ->
                            let n = length cards
                                bottomY = (origY - cardHeight) - overlapY * fromIntegral (n-1) 
                                ix = (length cards - 1) `min` floor (((origY + cardHeight) - y) / overlapY)
                                (left, taken) = splitAt ix cards
                            in  if ix >= 0 && y >= bottomY
                                    then Just (left, Bunch (positions !! ix) pt taken loc)
                                    else Nothing
                        _ -> Nothing
                ) eMouse cards
            eRemoveCards = fst <$> eMouseSelection   -- Cards left over when we drag
            eDrag        = snd <$> eMouseSelection   -- Cards removed when we drag
    let sprites = zipWith (\pos card -> draw card (pos, cardSize)) positions <$> cards
        dest = (\cards freeSpaces -> Destination {
                    deLocation = loc,
                    deDropZone = (orig `minus` (0, fromIntegral (length cards) * overlapY), cardSize),
                    deMayDrop = \newCards ->
                        validSequence newCards &&
                        -- You get one card for free, but there must be free cells for any
                        -- more than that.
                        (length newCards - 1) <= freeSpaces &&
                        case cards of
                            [] -> True
                            _  -> last cards `follows` head newCards
                }
            ) <$> cards <*> freeSpaces
    return (sprites, dest, eDrag)

-- | The "free cells" where cards can be temporarily put.
cell :: Platform p =>
        (Card -> Drawable p)
     -> Drawable p
     -> Event (MouseEvent p) -> Location -> Event [Card]
     -> Reactive (Behavior [Sprite p], Behavior Destination, Event Bunch, Behavior Int)
cell draw emptySpace eMouse loc@(Cell ix) eDrop = do
    let (cardWidth, cardHeight) = cardSize
        orig = ((-1000) + cardWidth*0.5 + fromIntegral ix * cardSpacingNarrow, topRow)
        rect = (orig, cardSize)
    rec
        mCard <- hold Nothing $ eRemove `merge` (Just . head <$> eDrop)
        let eMouseSelection = filterJust $ snapshotWith (\mev mCard ->
                    case (mev, mCard) of
                        (MouseDown _ pt, Just card) | pt `inside` rect ->
                            Just (Nothing, Bunch (fst rect) pt [card] loc)
                        _ -> Nothing
                ) eMouse mCard
            eRemove = fst <$> eMouseSelection
            eDrag = snd <$> eMouseSelection
    let sprites = ((:[]) . maybe (emptySpace (orig, cardSize)) (\card -> draw card (orig, cardSize))) <$> mCard
        dest = (\mCard -> Destination {
                deLocation = loc,
                deDropZone = rect,
                deMayDrop = \newCards -> length newCards == 1 && isNothing mCard
            }) <$> mCard
        emptySpaces = (\c -> if isNothing c then 1 else 0) <$> mCard
    return (sprites, dest, eDrag, emptySpaces)

-- | The place where the cards end up at the top right, aces first.
grave :: Platform p =>
         (Card -> Drawable p)
      -> Drawable p
      -> Event (MouseEvent p) -> Event [Card]
      -> Reactive (Behavior [Sprite p], Behavior Destination, Event Bunch)
grave draw emptySpace eMouse eDrop = do
    let xOf ix = 1000 - cardWidth*0.5 - cardSpacingNarrow * fromIntegral (3-ix)
        positions = map (\ix -> (xOf ix, topRow)) [0..3]
        areas = zip positions (repeat cardSize)
        (cardWidth, cardHeight) = cardSize
        wholeRect = (((xOf 0 + xOf 3) * 0.5, topRow), ((cardSpacingNarrow * 3 + cardWidth*2) * 0.5, cardHeight))    
    rec
        let eDropModify = snapshotWith (\newCards slots ->
                    let newCard@(Card _ suit) = head newCards
                        ix = fromEnum suit
                    in  take ix slots ++ [Just newCard] ++ drop (ix+1) slots 
                ) eDrop slots
        slots <- hold [Nothing, Nothing, Nothing, Nothing] (eDropModify `merge` eRemove)
        let eMouseSelection = filterJust $ snapshotWith (\mev slots ->
                    case mev of
                        MouseDown _ pt ->
                            let isIn = map (pt `inside`) areas
                            in  case trueIxOf isIn of
                                    Just ix ->
                                        case slots !! ix of
                                            Just card@(Card value suit) ->
                                                let prevCard = if value == Ace then Nothing
                                                                               else Just (Card (pred value) suit)
                                                    slots' = take ix slots ++ [prevCard] ++ drop (ix+1) slots
                                                in  Just (slots', Bunch (positions !! ix) pt [card] Grave)
                                            Nothing -> Nothing
                                    Nothing -> Nothing
                        _ -> Nothing
                ) eMouse slots
            eRemove = fst <$> eMouseSelection
            eDrag = snd <$> eMouseSelection
    let sprites = zipWith (\pos mSlot ->
                maybe (emptySpace (pos, cardSize)) (\card -> draw card (pos, cardSize)) mSlot
            ) positions <$> slots
        dest = (\slots -> Destination {
                deLocation = Grave,
                deDropZone = wholeRect,
                deMayDrop = \newCards -> case newCards of
                    [card@(Card value suit)] ->
                        let ix = fromEnum suit
                        in  case slots !! ix of
                                Just (Card topValue _) -> value == succ topValue
                                Nothing                -> value == Ace 
                    _                    -> False
            }) <$> slots
    return (sprites, dest, eDrag)
  where
    -- Index of first true item in the list
    trueIxOf items = doit items 0
      where
        doit [] _ = Nothing
        doit (x:xs) ix = if x then Just ix
                              else doit xs (ix+1)

-- | Draw the cards while they're being dragged.
dragger :: Platform p =>
           (Card -> Drawable p)
        -> Event (MouseEvent p) -> Event Bunch -> Reactive (Behavior [Sprite p], Event (Point, Bunch))
dragger draw eMouse eStartDrag = do
    dragPos <- hold (0,0) $ flip fmap eMouse $ \mev ->
        case mev of
            MouseUp   _ pt -> pt
            MouseMove _ pt -> pt
            MouseDown _ pt -> pt
    rec
        dragging <- hold Nothing $ (const Nothing <$> eDrop) `merge` (Just <$> eStartDrag)
        let eDrop = filterJust $ snapshotWith (\mev mDragging ->
                    case (mev, mDragging) of
                        -- If the mouse is released, and we are dragging...
                        (MouseUp _ pt, Just dragging) -> Just (cardPos pt dragging, dragging)
                        _                             -> Nothing
                ) eMouse dragging
    let sprites = drawDraggedCards <$> dragPos <*> dragging
          where
            drawDraggedCards pt (Just bunch) =
                let cpos = cardPos pt bunch
                    positions = iterate (\(x, y) -> (x, y-overlapY)) cpos
                in  zipWith (\card pos -> draw card (pos, cardSize)) (buCards bunch) positions
            drawDraggedCards _ Nothing = []
    return (sprites, eDrop)
  where
    cardPos pt bunch = (pt `minus` buInitMousePos bunch) `plus` buInitOrig bunch

-- | Determine where dropped cards are routed to.
dropper :: Event (Point, Bunch) -> Behavior [Destination] -> Event (Location, [Card])
dropper eDrop dests =
    snapshotWith (\(pt, bunch) dests ->
                -- If none of the destinations will accept the dropped cards, then send them
                -- back where they originated from.
                let findDest [] = (buOrigin bunch, buCards bunch)
                    findDest (dest:rem) =
                        if pt `inside` deDropZone dest && deMayDrop dest (buCards bunch)
                            then (deLocation dest, buCards bunch)
                            else findDest rem
                in  findDest dests
            ) eDrop dests

distributeTo :: Event (Location, [Card]) -> [Location] -> [Event [Card]]
distributeTo eWhere locations = flip map locations $ \thisLoc ->
    filterJust $ (\(loc, cards) ->
            if loc == thisLoc
                then Just cards
                else Nothing
        ) <$> eWhere

game :: Platform p =>
        (Card -> Drawable p)
     -> Drawable p
     -> Event (MouseEvent p)
     -> Behaviour Double
     -> StdGen
     -> Reactive (Behaviour (Sprite p))
game draw emptySpace eMouse time rng = do
    let stackCards =
            let (cards, _) = shuffle rng [minBound..maxBound]
            in  toStacks noOfStacks cards
        stLocs = map Stack [0..noOfStacks-1]
        ceLocs = map Cell [0..noOfCells-1]
    rec
        let eWhere = dropper eDrop (sequenceA (stDests ++ ceDests ++ [grDest]))
            stDrops = eWhere `distributeTo` stLocs
            ceDrops = eWhere `distributeTo` ceLocs
            grDrops = eWhere `distributeTo` [Grave]
        (stSprites, stDests, stDrags) <- unzip3 <$> forM (zip3 stLocs stackCards stDrops) (\(loc, cards, drop) ->
            stack draw eMouse cards loc emptySpaces drop)
        (ceSprites, ceDests, ceDrags, ceEmptySpaces) <- unzip4 <$> forM (zip ceLocs ceDrops) (\(loc, drop) ->
            cell draw emptySpace eMouse loc drop)
        (grSprites, grDest, grDrag) <- grave draw emptySpace eMouse (head grDrops)
        -- The total number of empty spaces available in cells - 0 to 4. We need to
        -- know this when we drop a stack of cards, because (the rules of the game say)
        -- this is equivalent to temporarily putting all but one of them in cells.
        let emptySpaces = foldr1 (\x y -> (+) <$> x <*> y) ceEmptySpaces
        (drSprites, eDrop) <- dragger draw eMouse (foldr1 merge (stDrags ++ ceDrags ++ [grDrag]))
    return $ mconcat . concat <$> sequenceA (stSprites ++ ceSprites ++ [grSprites] ++ [drSprites])

shuffle :: StdGen -> [Card] -> ([Card], StdGen)
shuffle rng cards =
    let n = length cards
        (rng', ixes) = mapAccumL (\rng () ->
                let (ix, rng') = randomR (0, n-1) rng
                in  (rng', ix)) rng (replicate n ())
        ary = runSTArray $ do
            ary <- newListArray (0, n-1) cards
            forM_ (zip [0..n-1] ixes) $ \(ix1, ix2) -> do
                when (ix1 /= ix2) $ do
                    one <- readArray ary ix1
                    two <- readArray ary ix2
                    writeArray ary ix1 two
                    writeArray ary ix2 one
            return ary
    in  (A.elems ary, rng')

toStacks :: Int -> [Card] -> [[Card]]
toStacks noOfStacks cards = foldl (\stacks layer ->
        zipWith (++) (map (:[]) layer ++ repeat []) stacks
    ) (replicate noOfStacks []) (layerize cards)
  where
    layerize :: [Card] -> [[Card]]
    layerize cards = case splitAt noOfStacks cards of
        ([], _) -> []
        (layer, rem) -> layer : layerize rem

