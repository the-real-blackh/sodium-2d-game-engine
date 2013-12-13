{-# LANGUAGE RecursiveDo, OverloadedStrings, TupleSections #-}
module Freecell (freecell, Background(..)) where

import Button

import FRP.Sodium.GameEngine2D.Geometry
import FRP.Sodium.GameEngine2D.Platform
import FRP.Sodium.GameEngine2D.Gesture
import FRP.Sodium
import Control.Applicative
import Control.Monad
import Data.Default
import Data.Traversable (sequenceA)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Sequence (Seq, (<|))
import qualified Data.Sequence as Seq 
import System.Random hiding (split)
import System.FilePath
import Data.Array.IArray as A
import Data.Array.ST
import Data.Text (Text)
import Debug.Trace


freecell :: Platform p => [Background FilePath] -> IO (GameInput p -> Reactive (GameOutput p))
freecell bgFiles = game <$> loadResources bgFiles

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

data Background r = Background {
        bgResource :: r,
        bgDuration :: Double
    }

data Resources p = Resources {
        reDraw        :: Card -> Drawable p,
        reEmptySpace  :: Drawable p,
        reBackgrounds :: [Background (Sprite p)],
        reHelpText    :: Drawable p,
        reNewGame     :: ButtonImage p,
        reRestart     :: ButtonImage p,
        reUndo        :: ButtonImage p,
        reHelp        :: ButtonImage p
    }

loadResources :: Platform p => [Background FilePath] -> IO (Resources p)
loadResources bgFiles =
    Resources <$> (do
                       cards <- forM [minBound..maxBound] $ \card -> do
                           i <- image (cardFn card)
                           return (card, i)
                       let cardsM = M.fromList cards
                           draw card = fromJust $ M.lookup card cardsM
                       return draw
                  )
              <*> image "empty-space.png"
              <*> (do
                       forM bgFiles $ \f -> do
                           i <- backgroundImage (bgResource f)
                           return $ Background i (bgDuration f)
                  )
              <*> image "help.png"
              <*> (ButtonImage <$> image "new-game.up.png" <*> image "new-game.dn.png" <*> pure (140 / 29))
              <*> (ButtonImage <$> image "restart.up.png"  <*> image "restart.dn.png"  <*> pure (108 / 29))
              <*> (ButtonImage <$> image "undo.up.png"     <*> image "undo.dn.png"     <*> pure (90 / 30))
              <*> (ButtonImage <$> image "help.up.png"     <*> image "help.dn.png"     <*> pure (84 / 30))

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

noOfGraves :: Int
noOfGraves = 4

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

data GameState = GameState {
        gsStacks0 :: [[Card]],  -- Cards stacks as at the start of the game
        gsStacks  :: [[Card]],  -- Card stacks now
        gsCells   :: [Maybe Card],
        gsGrave   :: [Maybe Card]
    }
    deriving (Eq, Show)

mkGameState :: [[Card]] -> GameState
mkGameState cards = GameState cards cards (replicate noOfCells Nothing) (replicate noOfGraves Nothing)

-- | The vertical stacks of cards, where cards can only be added if they're
-- descending numbers and alternating red-black.
stack :: Platform p =>
         Behavior Rect
      -> (Card -> Drawable p)
      -> Event (MouseEvent p)
      -> [Card]
      -> Location
      -> Behavior Int
      -> Event [Card]
      -> Event Point
      -> Behavior [Destination]
      -> Event [Card]
      -> Reactive (Behavior (Sprite p), Behavior Destination, Event Bunch,
              Behavior [Card], Event ((Location, [Card])))
stack board draw eMouse initCards loc@(Stack ix) freeSpaces eDrop eDblClick dblClickDests eSetState = do
    let orig board =
            let (cardWidth, cardHeight) = cardSize board
            in  (
                    xMin board + fromIntegral ix * cardSpacing board,
                    stackTop board
                )
        positions board = iterate (\(x, y) -> (x, y-overlapY board)) (orig board)
    rec
        cards <- hold initCards (eDragRemove `merge` eDblClkRemove `merge` eAddCards `merge` eSetState)
        let eAddCards = snapshot (\newCards cards -> cards ++ newCards) eDrop cards
            eMouseSelection = filterJust $ snapshot (\mev (cards, board) ->
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
            eDragRemove = fst <$> eMouseSelection   -- Cards left over when we drag
            eDrag        = snd <$> eMouseSelection   -- Cards removed when we drag
            eDblClickSelection = filterJust $ snapshot (\pt (cards, dests, board) ->
                    if null cards then Nothing else
                        let cardRect = (positions board !! (length cards - 1), cardSize board)
                        in  if pt `inside` cardRect
                                then 
                                    let cards' = take (length cards - 1) cards
                                        card = [cards !! (length cards - 1)]
                                        check [] = Nothing
                                        check (dest:dests) | deMayDrop dest card = Just (cards', (deLocation dest, card))
                                        check (_:dests) = check dests
                                    in  check dests
                                else Nothing
                ) eDblClick (liftA3 (,,) cards dblClickDests board)
            eDblClkRemove = fst <$> eDblClickSelection   -- Cards left over when we double click
            eDblClickDispatch = snd <$> eDblClickSelection -- Cards removed when we double click
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
    return (sprites, dest, eDrag, cards, eDblClickDispatch)

-- | The "free cells" where cards can be temporarily put.
cell :: Platform p =>
        Behavior Rect
     -> (Card -> Drawable p)
     -> Drawable p
     -> Event (MouseEvent p)
     -> Location
     -> Event [Card]
     -> Event (Maybe Card)
     -> Reactive (Behavior (Sprite p), Behavior Destination, Event Bunch, Behavior Int, Behavior (Maybe Card))
cell board draw emptySpace eMouse loc@(Cell ix) eDrop0 eSetState = do
    let orig board =
            let narrow = cardSpacingNarrow board
                (cardWidth, _) = cardSize board
            in  (xMin board + fromIntegral ix * narrow, topRow board)
        rect board = (orig board, cardSize board)
        eDrop = Just . head <$> eDrop0
    rec
        mCard <- hold Nothing $ eDragRemove `merge` eDrop `merge` eSetState

        let eMouseSelection = filterJust $ snapshot (\mev (mCard, board) ->
                    case (mev, mCard) of
                        (MouseDown _ pt, Just card) | pt `inside` rect board ->
                            Just (Nothing, Bunch (orig board) pt [card] loc)
                        _ -> Nothing
                ) eMouse (liftA2 (,) mCard board)
            eDragRemove = fst <$> eMouseSelection
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
    return (sprites, dest, eDrag, emptySpaces, mCard)

-- | The place where the cards end up at the top right, aces first.
grave :: Platform p =>
         Behavior Rect
      -> (Card -> Drawable p)
      -> Drawable p
      -> Event (MouseEvent p)
      -> Event [Card]
      -> Event [Maybe Card]
      -> Reactive (Behavior (Sprite p), Behavior Destination, Event Bunch,
                   Behavior [Maybe Card])
grave board draw emptySpace eMouse eDrop eSetState = do
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
        let eDropModify = snapshot (\newCards slots ->
                    let newCard@(Card _ suit) = head newCards
                        ix = fromEnum suit
                    in  take ix slots ++ [Just newCard] ++ drop (ix+1) slots 
                ) eDrop slots
        slots <- hold (replicate noOfGraves Nothing) (eDropModify `merge` eDragRemove `merge` eSetState)
        let eMouseSelection = filterJust $ snapshot (\mev (slots, board) ->
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
            eDragRemove = fst <$> eMouseSelection
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
    return (sprites, dest, eDrag, slots)
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
        let eDrop = filterJust $ snapshot (\mev mDragging ->
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
-- Bool value is True if this is a successful drag, false if it's a snap back to origin.
dropper :: Event (Point, Bunch) -> Behavior [Destination] -> Event (Bool, (Location, [Card]))
dropper eDrop dests =
    snapshot (\(pt, bunch) dests ->
                -- If none of the destinations will accept the dropped cards, then send them
                -- back where they originated from.
                let findDest [] = (False, (buOrigin bunch, buCards bunch))
                    findDest (dest:rem) =
                        if pt `inside` deDropZone dest && deMayDrop dest (buCards bunch)
                            then (True, (deLocation dest, buCards bunch))
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

maxUndos :: Int
maxUndos = 50

undoHandler :: Behavior GameState
            -> Event ()                 -- ^ Drag start
            -> Event ()                 -- ^ Drag end
            -> Event ()                 -- ^ Other state update (other than drag end)
            -> Event ()                 -- ^ Undo button clicked
            -> Reactive (Event GameState)
undoHandler state eDragStart eDragEnd eOtherUpdate eUndo = do
    capturedState <- hold Nothing $ Just <$> snapshot (flip const) eDragStart state
    rec
        undos <- hold Seq.empty (ePush `merge` (fst <$> ePop))

        let eUpdate = snapshot (flip const) eDragEnd capturedState `merge`
                      snapshot (flip const) eOtherUpdate (Just <$> state)

            ePush = snapshot (\mState undos ->
                    case mState of
                        Just state ->
                            if not (Seq.null undos) && Seq.index undos 0 == state
                                then undos
                                else Seq.take maxUndos (state <| undos)
                        Nothing    -> undos
                ) eUpdate undos

            ePop = filterJust $ snapshot (\_ undos ->
                    if Seq.null undos
                        then Nothing
                        else Just (Seq.drop 1 undos, Seq.index undos 0)
                ) eUndo undos

    return $ snd <$> ePop

helpPage :: Platform p =>
            Resources p
         -> Event ()
         -> Event (MouseEvent p)
         -> Behavior Rect
         -> Reactive (Behavior (Sprite p), Event (MouseEvent p))
helpPage res eHelp eMouse screen = do
    active <- hold False $ (const True <$> eHelp)
              `merge` (filterJust $ fmap (\me -> case me of
                                                     MouseDown _ _ -> Just False
                                                     _             -> Nothing) eMouse)
    return (liftA2 draw active screen, gateMouse eMouse (not <$> active))
  where
    draw active screen =
        if active
            then reHelpText res $ fitAspect (640/400) CentreJ
                            $ dropLeftP xmar $ dropRightP xmar
                            $ dropTopP  ymar $ dropBottomP ymar $ screen
            else mempty
    xmar = 10
    ymar = 10

backgroundChanger :: Platform p =>
                     Resources p
                  -> Behavior Double
                  -> Behavior Coord
                  -> Reactive (Behavior (Sprite p))
backgroundChanger res time aspect = do
    t0 <- sample time
    rec
        bgs <- hold (t0, cycle (reBackgrounds res)) eChange
        let eChange = filterJust $ snapshot (\t (t0, bgs@(bg:bgs')) ->
                              if t - t0 >= bgDuration bg
                                  then Just (t, bgs')
                                  else Nothing
                          ) (updates time) bgs
    return $ liftA2 (\(_, (bg1:bg2:_)) aspect ->
        let screen = ((0,0),(aspect*1000,1000))
        in
            bgResource bg1 `mappend`
            -- display the second one invisibly so it'll be loaded by the time we get to it
            invisible (bgResource bg2)
      ) bgs aspect

game :: Platform p =>
        Resources p
     -> GameInput p
     -> Reactive (GameOutput p)
game res GameInput { giAspect = aspect, giMouse = eMouse0, giTime = time, giRNG0 = rng0 } = do

    bgSpr <- backgroundChanger res time aspect

    let screen = flip fmap aspect $ \aspect -> marginRect 90 ((0,0),(1000*(min 1.3 aspect),1000))
        buttonArea = takeTopP 50 . takeTopP 9 <$> screen
        board = takeBottomP 91 <$> screen
        draw = reDraw res
        emptySpace = reEmptySpace res
        mkStackCards rng0 =
            let (cards, rng1) = shuffle rng0 [minBound..maxBound]
            in  (toStacks noOfStacks cards, rng1)
        (stackCards00, rng1) = mkStackCards rng0
        stLocs = map Stack [0..noOfStacks-1]
        ceLocs = map Cell [0..noOfCells-1]

        newGameRect = fitAspect (biAspect (reNewGame res)) LeftJ <$> buttonArea
        buttonArea' = (\ng -> dropLeft (rectWidth ng) . dropLeft 50) <$> newGameRect <*> buttonArea
        restartRect = fitAspect (biAspect (reRestart res)) LeftJ <$> buttonArea'
        buttonArea'' = (\ng -> dropLeft (rectWidth ng) . dropLeft 50) <$> restartRect <*> buttonArea'
        undoRect    = fitAspect (biAspect (reUndo res)) LeftJ <$> buttonArea''
        buttonArea''' = (\ng -> dropLeft (rectWidth ng) . dropLeft 50) <$> undoRect <*> buttonArea''
        helpRect    = fitAspect (biAspect (reHelp res)) RightJ <$> buttonArea'''

    rec
        (helpPageSpr, eMouse1) <- helpPage res eHelp eMouse0 screen
        (eDblClick, eMouse) <- doubleClickGesture (pure (const True)) eMouse1 time
        (helpSpr, eHelp, helpSnd) <- button (reHelp res) helpRect eMouse1

    (newGameSpr, eNewGame, newGameSnd) <- button (reNewGame res) newGameRect eMouse1
    (restartSpr, eRestart, restartSnd) <- button (reRestart res) restartRect eMouse1
    (undoSpr,    eUndo,    undoSnd)    <- button (reUndo res) undoRect eMouse1
    let buttonsSpr = mconcat <$> sequenceA [newGameSpr, restartSpr, undoSpr, helpSpr]

    eNewGameSt <- collectE (\() rng -> mkStackCards rng) rng1 eNewGame

    rec
        -- The cards as they were at the start of this game.
        stackCards0 <- hold stackCards00 $ eNewGameSt `merge` (gsStacks0 <$> eSetState)

        let eWhereSucc = dropper eDrop (sequenceA (stDests ++ ceDests ++ [grDest]))
            eWhere  = (snd <$> eWhereSucc) `merge` eDblClickDispatches
            stDrops = eWhere `distributeTo` stLocs
            ceDrops = eWhere `distributeTo` ceLocs
            grDrops = eWhere `distributeTo` [Grave]
            eSuccessfulDrop = filterJust $ fmap (\(success, _) ->
                if success then Just () else Nothing) eWhereSucc

        let eOtherUndoableEvents = (const () <$> eDblClickDispatches)
                       `merge` (const () <$> eNewGameSt `merge` eRestartSt)
        eUndoSt <- undoHandler gameState (const () <$> eDrag) eSuccessfulDrop
                                          eOtherUndoableEvents eUndo
        let eRestartSt = snapshot (flip const) eRestart stackCards0
            eSetState = eUndoSt `merge` (mkGameState <$> (eRestartSt `merge` eNewGameSt))

        let eSetStackStates = map (\i -> (!! i) . gsStacks <$> eSetState) [0..noOfStacks-1]
        (stSprites, stDests, stDrags, stStates, stDblClickDispatches) <-
            unzip5 <$> forM (zip4 stLocs stackCards00 stDrops eSetStackStates) (\(loc, cards, drop, eSet) ->
                stack board draw eMouse cards loc emptySpaces drop eDblClick (sequenceA (grDest:ceDests)) eSet)

        -- Cards dispatched to places at the top of the screen because of a double click
        let eDblClickDispatches = foldr1 merge stDblClickDispatches

        let eSetCellStates = map (\i -> (!! i) . gsCells <$> eSetState) [0..noOfCells-1]
        (ceSprites, ceDests, ceDrags, ceEmptySpaces, ceStates) <-
            unzip5 <$> forM (zip3 ceLocs ceDrops eSetCellStates) (\(loc, drop, eSet) ->
                cell board draw emptySpace eMouse loc drop eSet)

        (grSprites, grDest, grDrag, grState) <-
            grave board draw emptySpace eMouse (head grDrops) (gsGrave <$> eSetState)

        let gameState = GameState <$> stackCards0
                                  <*> sequenceA stStates
                                  <*> sequenceA ceStates
                                  <*> grState

        -- The total number of empty spaces available in cells - 0 to 4. We need to
        -- know this when we drop a stack of cards, because (the rules of the game say)
        -- this is equivalent to temporarily putting all but one of them in cells.
        let emptySpaces = foldr1 (\x y -> (+) <$> x <*> y) ceEmptySpaces
            eDrag = foldr1 merge (stDrags ++ ceDrags ++ [grDrag])
        (drSprites, eDrop) <- dragger board draw eMouse eDrag
    return $ def {
            goSprite = mconcat <$> sequenceA (
                [bgSpr, buttonsSpr] ++
                stSprites ++
                ceSprites ++
                [grSprites, drSprites, helpPageSpr]
            )
        }

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
