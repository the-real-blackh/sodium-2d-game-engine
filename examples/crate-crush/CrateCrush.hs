{-# LANGUAGE RecursiveDo, OverloadedStrings, TupleSections #-}
module CrateCrush (crateCrush) where

import FRP.Sodium.GameEngine2D.Geometry
import FRP.Sodium.GameEngine2D.Platform
import FRP.Sodium
import Control.Applicative
import Control.Monad (mplus)
import Data.Default
import Data.List (foldr1)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable
import System.FilePath
import System.Random


crateCrush :: Platform p => Args p -> IO (GameInput p -> Reactive (GameOutput p))
crateCrush args = do
    crate <- image "crate.png"
    grass <- image "grass.png"
    create <- sound "create.ogg"
    smash <- sound "smash.ogg"
    retainSound args create
    retainSound args smash
    return $ game crate grass create smash

-- | Pair each event occurrence with a unique id
identify :: Event a -> Reactive (Event (Int, a))
identify ea = do
    ident <- accum 0 (const (1+) <$> ea)
    return $ snapshot (\a ident -> (ident, a)) ea ident

game :: Platform p =>
        Drawable p           -- ^ Crate
     -> Drawable p           -- ^ Grass
     -> Sound p              -- ^ Create
     -> Sound p              -- ^ Smash
     -> GameInput p
     -> Reactive (GameOutput p)
game drawCrate drawGrass playCreate playDestroy
        GameInput { giMouse = eMouse, giTime = time } = do

    rec
        -- Behavior (Map Int (Behavior Rect))
        crateMap <- accum M.empty (merge eCreate eDestroy)
        -- Behavior [Behavior (Int, Rect)]
        let crates0 = fmap (map (\(i, b) -> fmap (i, ) b) . M.toList) crateMap
        -- Behavior (Behavior [(Int, Rect)])
        let crates1 = fmap sequenceA crates0
        -- Behavior [(Int, Rect)]
        crates <- switch crates1
        -- Behavior [Rect]
        let rects = fmap (map snd) crates

        -- Event Point
        let eNewPoint = filterJust $ snapshot (\mev rects ->
                    case mev of
                        MouseDown _ pt | not (any (pt `inside`) rects) -> Just pt
                        _ -> Nothing
                ) eMouse rects

        -- Event (Int, Point)
        eIdNewPoint <- identify eNewPoint

        -- Event (Int, Behavior Rect)
        let eIdNewCrate = execute $ fmap (\(i, pt) -> do
                                              rect <- crate i pt crates time
                                              return (i, rect)
                                         ) eIdNewPoint
        let eCreate = fmap (\(id, crate) -> M.insert id crate) eIdNewCrate

        let eDestroy = filterJust $ snapshot (\mev crates ->
                            case mev of
                                MouseDown _ pt ->
                                    case filter (\(ident, rect) -> pt `inside` rect) crates of
                                        ((ident, rect):_) -> Just $ M.delete ident
                                        _ -> Nothing
                                _ -> Nothing
                        ) eMouse crates

    let eSound = merge
            (fmap (const playCreate) eCreate)
            (fmap (const playDestroy) eDestroy)
        sprites =
            fmap (mconcat . (drawGrass grassRect:) . map drawCrate) rects

    return $ def { goSprite = sprites, goEffects = eSound }

crateSize :: Vector
crateSize = (120,120)

gravity :: Coord
gravity = -2

grassRect :: Rect
grassRect = edgesRect (-5000, -1100, 5000, (-1000) + crateHt)
  where (_, crateHt) = crateSize

sitsOn :: Rect -> Rect -> Maybe Coord
sitsOn r0 r1 =
    let (ax0, ay0, ax1, ay1) = rectEdges r0  -- top rect
        (bx0, by0, bx1, by1) = rectEdges r1  -- bottom rect
    in
        if ax1 > bx0 &&
           ax0 < bx1 then
           let belowBy = by1 - ay0
           in  if belowBy < 0 then Nothing
                              else Just belowBy
        else Nothing
  where (_, crateHt) = crateSize

crate :: Int                     -- ^ Unique identifier
      -> Point                   -- ^ Initial position
      -> Behavior [(Int, Rect)]  -- ^ Positions of all crates 
      -> Behavior Double        -- ^ Time
      -> Reactive (Behavior Rect)
crate me pt0 crates time = do
    let obstacles = fmap (\crates ->
                -- add the grass to the list of obstacles
                [grassRect] ++
                -- exclude myself
                mapMaybe (\(i, rect) -> if i /= me then Just rect else Nothing) crates
            ) crates
        eDeltaT = fmap realToFrac (delta (flip subtract) time)

    rec
        let eAccel = snapshot (\dt ((vel, sitting, _), pos, obstacles) ->
                        let delta = (0, gravity * dt)
                            vel' = vel `plus` delta
                            pos' = pos `plus` (scale (1/dt) vel')
                            rect = (pos, crateSize)
                            rect' = (pos', crateSize)
                            goingBelow obst = case (rect `sitsOn` obst, rect' `sitsOn` obst) of
                                (Nothing, Just belowBy) -> Just belowBy
                                _                       -> Nothing
                        in  if not sitting then
                                case foldr1 mplus (map goingBelow obstacles) of
                                    Just belowBy -> (delta `plus` (0, belowBy*dt), True, dt)
                                    Nothing      -> (delta, False, dt)
                            else
                                case foldr1 mplus (map (rect' `sitsOn`) obstacles) of
                                    Just belowBy | belowBy < crateHt*2 -> (negateVector vel, True, dt)
                                    _ -> (delta, False, dt)
                    ) eDeltaT (liftA3 (,,) velocity position obstacles)
        velocity <- integrate velPlus ((0, 0), False, 0) eAccel
        position <- integrate plus pt0 $ fmap (\(vel,_,dt) -> scale (1/dt) vel) (updates velocity)
    return (fmap (,crateSize) position)
  where (_, crateHt) = crateSize

delta :: (a -> a -> a) -> Behavior a -> Event a
delta minus a = snapshot minus (updates a) a

velPlus :: (Point, Bool, Coord) -> (Point, Bool, Coord) -> (Point, Bool, Coord)
velPlus (a, _, _) (b, sitting, dt) = (a `plus` b, sitting, dt)

integrate :: (a -> a -> a) -> a -> Event a -> Reactive (Behavior a)
integrate plus orig = accum orig . fmap (flip plus) 

