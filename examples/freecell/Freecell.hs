{-# LANGUAGE RecursiveDo, OverloadedStrings, TupleSections #-}
module Freecell (freecell) where

import Button

import FRP.Sodium.GameEngine2D.Geometry
import FRP.Sodium.GameEngine2D.Platform
import FRP.Sodium
import Control.Applicative
import Control.Monad
import Data.Traversable (sequenceA)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import System.Random
import System.FilePath
import Data.Array.IArray as A
import Data.Array.ST
import Data.Text (Text)


freecell :: Platform p => IO (Behavior Coord -> Game p)
freecell = game <$> loadResources

data ButtonImage p = ButtonImage {
        biUp :: Drawable p,
        biDown :: Drawable p,
        biAspect :: Coord
    }

button :: Platform p =>
          ButtonImage p 
       -> Behavior Rect
       -> Event (MouseEvent p)
       -> Reactive (Behavior (Sprite p), Event (), Event ())
button bi rect eMouse = plainButton (pure Enabled) rect draw eMouse
  where
    draw b = flip fmap b $ \(rect, _, sel) ->
        if sel then biDown bi rect `mappend` invisible (biUp bi rect)
               else biUp bi rect `mappend` invisible (biDown bi rect)

data Resources p = Resources {
        reDraw       :: Card -> Drawable p,
        reEmptySpace :: Drawable p,
        reBackground :: Drawable p,
        reNewGame    :: ButtonImage p,
        reRestart    :: ButtonImage p,
        reUndo       :: ButtonImage p
    }

loadResources :: Platform p => IO (Resources p)
loadResources =
    Resources <$> (do
                       cards <- forM [minBound..maxBound] $ \card -> do
                           i <- image (cardFn card)
                           return (card, i)
                       let cardsM = M.fromList cards
                           draw card = fromJust $ M.lookup card cardsM
                       return draw
                  )
              <*>image "empty-space.png"
              <*> image "background.jpg"
              <*> (ButtonImage <$> image "new-game.up.png" <*> image "new-game.dn.png" <*> pure (140 / 29))
              <*> (ButtonImage <$> image "restart.up.png"  <*> image "restart.dn.png"  <*> pure (108 / 29))
              <*> (ButtonImage <$> image "undo.up.png"     <*> image "undo.dn.png"     <*> pure (90 / 30))

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
cardFn (Card v s) = suitName s ++ valueName v ++ ".png"
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

noOfStacks :: Int
noOfStacks = 8

noOfCells :: Int
noOfCells = 4

cardSize :: Rect -> Vector
cardSize board = (0.0495 * w, 0.07425 * w)
  where w = rectWidth board

maxCardsPerStack = 14

overlapY :: Rect -> Coord
overlapY board = (stackTop board - (y0 + topMargin + cardHeight)) / (maxCardsPerStack - 1)
  where
    (_, y0, _, _) = rectEdges board
    (_, cardHeight) = cardSize board

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

xMin :: Rect -> Coord
xMin board = left + leftMargin board + cardWidth
  where
    (left, _, _, _) = rectEdges board
    (cardWidth, _) = cardSize board

xMax :: Rect -> Coord
xMax board = right - leftMargin board - cardWidth
  where
    (_, _, right, _) = rectEdges board
    (cardWidth, _) = cardSize board

cardSpacing :: Rect -> Coord
cardSpacing board = (xMax board - xMin board) / fromIntegral (noOfStacks-1)
  where
    (cardWidth, _) = cardSize board

cardSpacingNarrow :: Rect -> Coord
cardSpacingNarrow board = cardSpacing board * 0.95

topMargin :: Coord
topMargin = 0

leftMargin :: Rect -> Coord
leftMargin board = 0

topRow :: Rect -> Coord
topRow board = top - topMargin - cardHeight
  where
    (_, _, _, top) = rectEdges board
    (cardWidth, cardHeight) = cardSize board

stackTop :: Rect -> Coord
stackTop board = topRow board - cardHeight * 2.5
  where (_, cardHeight) = cardSize board

-- | The vertical stacks of cards, where cards can only be added if they're
-- descending numbers and alternating red-black.
stack :: Platform p =>
         Behavior Rect
      -> (Card -> Drawable p)
      -> Event (MouseEvent p) -> [Card] -> Location -> Behavior Int -> Event [Card]
      -> Reactive (Behavior (Sprite p), Behavior Destination, Event Bunch)
stack board draw eMouse initCards loc@(Stack ix) freeSpaces eDrop = do
    let orig board =
            let (cardWidth, cardHeight) = cardSize board
            in  (
                    xMin board + fromIntegral ix * cardSpacing board,
                    stackTop board
                )
        positions board = iterate (\(x, y) -> (x, y-overlapY board)) (orig board)
    rec
        cards <- hold initCards (eRemoveCards `merge` eAddCards)
        let eAddCards = snapshotWith (\newCards cards -> cards ++ newCards) eDrop cards
            eMouseSelection = filterJust $ snapshotWith (\mev (cards, board) ->
                    let (cardWidth, cardHeight) = cardSize board
                        (origX, origY) = orig board
                    in  case mev of
                        MouseDown _ pt@(x, y) | x >= origX - cardWidth && x <= origX + cardWidth ->
                            let n = length cards
                                bottomY = (origY - cardHeight) - overlapY board * fromIntegral (n-1) 
                                ix = (length cards - 1) `min` floor (((origY + cardHeight) - y) / overlapY board)
                                (left, taken) = splitAt ix cards
                            in  if ix >= 0 && y >= bottomY
                                    then Just (left, Bunch (positions board !! ix) pt taken loc)
                                    else Nothing
                        _ -> Nothing
                ) eMouse (liftA2 (,) cards board)
            eRemoveCards = fst <$> eMouseSelection   -- Cards left over when we drag
            eDrag        = snd <$> eMouseSelection   -- Cards removed when we drag
    let sprites = (\cards board ->
                        mconcat $ zipWith (\pos card -> draw card (pos, cardSize board)) (positions board) cards
                  ) <$> cards <*> board
        dest = (\cards freeSpaces board -> Destination {
                    deLocation = loc,
                    deDropZone = (orig board `minus` (0, fromIntegral (length cards) * overlapY board), cardSize board),
                    deMayDrop = \newCards ->
                        validSequence newCards &&
                        -- You get one card for free, but there must be free cells for any
                        -- more than that.
                        (length newCards - 1) <= freeSpaces &&
                        case cards of
                            [] -> True
                            _  -> last cards `follows` head newCards
                }
            ) <$> cards <*> freeSpaces <*> board
    return (sprites, dest, eDrag)

-- | The "free cells" where cards can be temporarily put.
cell :: Platform p =>
        Behavior Rect
     -> (Card -> Drawable p)
     -> Drawable p
     -> Event (MouseEvent p) -> Location -> Event [Card]
     -> Reactive (Behavior (Sprite p), Behavior Destination, Event Bunch, Behavior Int)
cell board draw emptySpace eMouse loc@(Cell ix) eDrop = do
    let orig board =
            let narrow = cardSpacingNarrow board
                (cardWidth, _) = cardSize board
            in  (xMin board + fromIntegral ix * narrow, topRow board)
        rect board = (orig board, cardSize board)
    rec
        mCard <- hold Nothing $ eRemove `merge` (Just . head <$> eDrop)
        let eMouseSelection = filterJust $ snapshotWith (\mev (mCard, board) ->
                    case (mev, mCard) of
                        (MouseDown _ pt, Just card) | pt `inside` rect board ->
                            Just (Nothing, Bunch (orig board) pt [card] loc)
                        _ -> Nothing
                ) eMouse (liftA2 (,) mCard board)
            eRemove = fst <$> eMouseSelection
            eDrag = snd <$> eMouseSelection
    let sprites = (\mCard board -> case mCard of
                                       Just card -> draw card (orig board, cardSize board)
                                       Nothing   -> emptySpace (orig board, cardSize board)
                  ) <$> mCard <*> board
        dest = (\mCard board -> Destination {
                deLocation = loc,
                deDropZone = rect board,
                deMayDrop = \newCards -> length newCards == 1 && isNothing mCard
            }) <$> mCard <*> board
        emptySpaces = (\c -> if isNothing c then 1 else 0) <$> mCard
    return (sprites, dest, eDrag, emptySpaces)

-- | The place where the cards end up at the top right, aces first.
grave :: Platform p =>
         Behavior Rect
      -> (Card -> Drawable p)
      -> Drawable p
      -> Event (MouseEvent p) -> Event [Card]
      -> Reactive (Behavior (Sprite p), Behavior Destination, Event Bunch)
grave board draw emptySpace eMouse eDrop = do
    let xOf board ix =    let (cardWidth, _) = cardSize board
                              in  xMax board - cardSpacingNarrow board * fromIntegral (3-ix)
        positions board = map (\ix -> (xOf board ix, topRow board)) [0..3]
        areas board = zip (positions board) (repeat $ cardSize board)
        wholeRect board = let (cardWidth, cardHeight) = cardSize board
                              in  (
                                      ((xOf board 0 + xOf board 3) * 0.5, topRow board),
                                      ((cardSpacingNarrow board * 3 + cardWidth*2) * 0.5, cardHeight)
                                  ) 
    rec
        let eDropModify = snapshotWith (\newCards slots ->
                    let newCard@(Card _ suit) = head newCards
                        ix = fromEnum suit
                    in  take ix slots ++ [Just newCard] ++ drop (ix+1) slots 
                ) eDrop slots
        slots <- hold [Nothing, Nothing, Nothing, Nothing] (eDropModify `merge` eRemove)
        let eMouseSelection = filterJust $ snapshotWith (\mev (slots, board) ->
                    case mev of
                        MouseDown _ pt ->
                            let isIn = map (pt `inside`) (areas board)
                            in  case trueIxOf isIn of
                                    Just ix ->
                                        case slots !! ix of
                                            Just card@(Card value suit) ->
                                                let prevCard = if value == Ace then Nothing
                                                                               else Just (Card (pred value) suit)
                                                    slots' = take ix slots ++ [prevCard] ++ drop (ix+1) slots
                                                in  Just (slots', Bunch (positions board !! ix) pt [card] Grave)
                                            Nothing -> Nothing
                                    Nothing -> Nothing
                        _ -> Nothing
                ) eMouse (liftA2 (,) slots board)
            eRemove = fst <$> eMouseSelection
            eDrag = snd <$> eMouseSelection
    let sprites = (
            \board slots ->
                mconcat $ zipWith (\pos mSlot ->
                         maybe (emptySpace (pos, cardSize board)) (\card -> draw card (pos, cardSize board)) mSlot)
                    (positions board) slots
            ) <$> board <*> slots
        dest = (\slots board -> Destination {
                deLocation = Grave,
                deDropZone = wholeRect board,
                deMayDrop = \newCards -> case newCards of
                    [card@(Card value suit)] ->
                        let ix = fromEnum suit
                        in  case slots !! ix of
                                Just (Card topValue _) -> value == succ topValue
                                Nothing                -> value == Ace 
                    _                    -> False
            }) <$> slots <*> board 
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
           Behavior Rect
        -> (Card -> Drawable p)
        -> Event (MouseEvent p) -> Event Bunch -> Reactive (Behavior (Sprite p), Event (Point, Bunch))
dragger board draw eMouse eStartDrag = do
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
    let sprites = drawDraggedCards <$> dragPos <*> dragging <*> board
          where
            drawDraggedCards pt (Just bunch) board =
                let cpos = cardPos pt bunch
                    positions = iterate (\(x, y) -> (x, y-overlapY board)) cpos
                in  mconcat $ zipWith (\card pos -> draw card (pos, cardSize board)) (buCards bunch) positions
            drawDraggedCards _ Nothing _ = mempty
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
        Resources p
     -> Behavior Coord  -- ^ Aspect ratio
     -> Event (MouseEvent p)
     -> Behaviour Double
     -> StdGen
     -> Reactive (
            Behaviour (Sprite p),
            Behavior (Text, [Sound p]),
            Event (Sound p)
        )
game res aspect eMouse time rng = do
    let screen = flip fmap aspect $ \aspect -> marginRect 90 ((0,0),(1000*(min 1.3 aspect),1000))
        buttonArea = takeTopP 50 . takeTopP 9 <$> screen
        board = takeBottomP 91 <$> screen
        draw = reDraw res
        emptySpace = reEmptySpace res
        stackCards =
            let (cards, _) = shuffle rng [minBound..maxBound]
            in  toStacks noOfStacks cards
        stLocs = map Stack [0..noOfStacks-1]
        ceLocs = map Cell [0..noOfCells-1]

        newGameRect = fitAspect (biAspect (reNewGame res)) LeftJ <$> buttonArea
        buttonArea' = (\ng -> dropLeft (rectWidth ng) . dropLeft 50) <$> newGameRect <*> buttonArea
        restartRect = fitAspect (biAspect (reRestart res)) LeftJ <$> buttonArea'
        buttonArea'' = (\ng -> dropLeft (rectWidth ng) . dropLeft 50) <$> restartRect <*> buttonArea'
        undoRect    = fitAspect (biAspect (reUndo res)) LeftJ <$> buttonArea''

    (newGameSpr, eNewGame, newGameSnd) <- button (reNewGame res) newGameRect eMouse
    (restartSpr, eRestart, restartSnd) <- button (reRestart res) restartRect eMouse
    (undoSpr,    eUndo,    undoSnd)    <- button (reUndo res) undoRect eMouse
    let buttonsSpr = liftA3 (\a b c -> a `mappend` b `mappend` c) newGameSpr restartSpr undoSpr

    rec
        let eWhere = dropper eDrop (sequenceA (stDests ++ ceDests ++ [grDest]))
            stDrops = eWhere `distributeTo` stLocs
            ceDrops = eWhere `distributeTo` ceLocs
            grDrops = eWhere `distributeTo` [Grave]
        (stSprites, stDests, stDrags) <- unzip3 <$> forM (zip3 stLocs stackCards stDrops) (\(loc, cards, drop) ->
            stack board draw eMouse cards loc emptySpaces drop)
        (ceSprites, ceDests, ceDrags, ceEmptySpaces) <- unzip4 <$> forM (zip ceLocs ceDrops) (\(loc, drop) ->
            cell board draw emptySpace eMouse loc drop)
        (grSprites, grDest, grDrag) <- grave board draw emptySpace eMouse (head grDrops)
        -- The total number of empty spaces available in cells - 0 to 4. We need to
        -- know this when we drop a stack of cards, because (the rules of the game say)
        -- this is equivalent to temporarily putting all but one of them in cells.
        let emptySpaces = foldr1 (\x y -> (+) <$> x <*> y) ceEmptySpaces
        (drSprites, eDrop) <- dragger board draw eMouse (foldr1 merge (stDrags ++ ceDrags ++ [grDrag]))
    return (
        mconcat
        . (reBackground res ((0,0),(bgAspect * 1000, 1000)):)
        <$> sequenceA ([buttonsSpr] ++ stSprites ++ ceSprites ++ [grSprites, drSprites]),
        pure ("", []),
        never
      )
  where
    bgAspect = 900 / 456

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

