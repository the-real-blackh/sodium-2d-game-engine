module FRP.Sodium.GameEngine2D.Geometry where


type Coord = Float
type Point = (Coord, Coord)
type Vector = (Coord, Coord)
type Rect = (Point, Vector)   -- Central point and size from centre to edge

scale :: Coord -> Vector -> Vector
scale s (vx, vy) = (vx*s, vy*s)

negateVector :: Vector -> Vector
negateVector (vx, vy) = (-vx, -vy)

distance :: Point -> Point -> Coord
distance (x0,y0) (x1,y1) = sqrt ((x1-x0)^2 + (y1-y0)^2)

plus :: Point -> Vector -> Point
plus (x0, y0) (x1, y1) = (x0+x1, y0+y1)

minus :: Point -> Point -> Vector
minus (x0, y0) (x1, y1) = (x0 - x1, y0 - y1)

translateRect :: Vector -> Rect -> Rect
translateRect v (orig, size) = (orig `plus` v, size)

scaleRect :: Vector -> Rect -> Rect
scaleRect (sx, sy) (o, (w, h)) = (o, (sx*w, sy*h))

marginRect :: Coord -> Rect -> Rect
marginRect m rect = scaleRect ((w-m)/w, (h-m)/h) rect
  where w = rectWidth rect
        h = rectHeight rect

rectWidth :: Rect -> Coord
rectWidth (_, (w, _)) = w*2

rectHeight :: Rect -> Coord
rectHeight (_, (_, h)) = h*2

rectAspect :: Rect -> Coord
rectAspect (_, (w, h)) = w/h

rectOrig :: Rect -> Point
rectOrig (orig, _) = orig

rectSize :: Rect -> Vector
rectSize (_, size) = size

-- | Drop the specified amount off the left of the rectangle.
dropLeft :: Coord -> Rect -> Rect
dropLeft chop rect = translateRect (chop*0.5, 0) . scaleRect ((w-chop)/w, 1) $ rect
  where w = rectWidth rect

dropLeftP :: Float -> Rect -> Rect
dropLeftP p rect = dropLeft (p * w * 0.01) rect
  where w = rectWidth rect

takeLeft :: Coord -> Rect -> Rect
takeLeft chop rect = dropRight (w - chop) rect
  where w = rectWidth rect

takeLeftP :: Float -> Rect -> Rect
takeLeftP p rect = takeLeft (p * w * 0.01) rect
  where w = rectWidth rect

-- | Drop the specified amount off the right of the rectangle.
dropRight :: Coord -> Rect -> Rect
dropRight chop rect = translateRect (-chop*0.5, 0) . scaleRect ((w-chop)/w, 1) $ rect
  where w = rectWidth rect

dropRightP :: Float -> Rect -> Rect
dropRightP p rect = dropRight (p * w * 0.01) rect
  where w = rectWidth rect

takeRight :: Coord -> Rect -> Rect
takeRight chop rect = dropLeft (w - chop) rect  
  where w = rectWidth rect

takeRightP :: Float -> Rect -> Rect
takeRightP p rect = takeRight (p * w * 0.01) rect  
  where w = rectWidth rect

-- | Drop the specified amount off the bottom of the rectangle.
dropBottom :: Coord -> Rect -> Rect
dropBottom chop rect = translateRect (0, chop*0.5) . scaleRect (1, (h-chop)/h) $ rect
  where h = rectHeight rect

dropBottomP :: Float -> Rect -> Rect
dropBottomP p rect = dropBottom (p * h * 0.01) rect  
  where h = rectHeight rect

takeBottom :: Coord -> Rect -> Rect
takeBottom chop rect = dropTop (h - chop) rect
  where h = rectHeight rect

takeBottomP :: Float -> Rect -> Rect
takeBottomP p rect = takeBottom (p * h * 0.01) rect  
  where h = rectHeight rect

-- | Drop the specified amount off the top of the rectangle.
dropTop :: Coord -> Rect -> Rect
dropTop chop rect = translateRect (0, -chop*0.5) . scaleRect (1, (h-chop)/h) $ rect
  where h = rectHeight rect

dropTopP :: Float -> Rect -> Rect
dropTopP p rect = dropTop (p * h * 0.01) rect  
  where h = rectHeight rect

takeTop :: Coord -> Rect -> Rect
takeTop chop rect = dropBottom (h - chop) rect
  where h = rectHeight rect

takeTopP :: Float -> Rect -> Rect
takeTopP p rect = takeTop (p * h * 0.01) rect  
  where h = rectHeight rect

-- | Chop /chop/ off the left of the rectangle, returning the left and the remainder (right).
splitLeft :: Coord -> Rect -> (Rect, Rect)
splitLeft chop rect = (takeLeft chop rect, dropLeft chop rect)

splitLeftP :: Float -> Rect -> (Rect, Rect)
splitLeftP p rect = splitLeft (p * w * 0.01) rect
  where w = rectWidth rect

-- | Chop /chop/ off the right of the rectangle, returning right and the remainder (left).
splitRight :: Coord -> Rect -> (Rect, Rect)
splitRight chop rect = (takeRight chop rect, dropRight chop rect)

splitRightP :: Float -> Rect -> (Rect, Rect)
splitRightP p rect = splitRight (p * w * 0.01) rect
  where w = rectWidth rect

-- | Chop /chop/ off the bottom of the rectangle, returning the bottom and the remainder (top).
splitBottom :: Coord -> Rect -> (Rect, Rect)
splitBottom chop rect = (takeBottom chop rect, dropBottom chop rect)

splitBottomP :: Float -> Rect -> (Rect, Rect)
splitBottomP p rect = splitBottom (p * h * 0.01) rect
  where h = rectHeight rect

-- | Chop /chop/ off the top of the rectangle, returning the top and the remainder (bottom).
splitTop :: Coord -> Rect -> (Rect, Rect)
splitTop chop rect = (takeTop chop rect, dropTop chop rect)

splitTopP :: Float -> Rect -> (Rect, Rect)
splitTopP p rect = splitTop (p * h * 0.01) rect
  where h = rectHeight rect

-- | Split a rectangle vertically into the specified percentages
splitVertical :: [Float] -> Rect -> [Rect]
splitVertical ps rect = go ps rect (sum ps)
  where
    go [] _ _ = error "splitVertical: empty list"
    go [_] rect _ = [rect]
    go (p:ps) rect total =
        let h = rectHeight rect
            (top, bottom) = splitTop (h * p / total) rect
        in  top : go ps bottom (total - p) 

data Justify = LeftJ | CentreJ | RightJ

-- | The resulting rectangle will have the specified aspect ratio and fit in the
-- specified rectangle.
fitAspect :: Coord -> Justify -> Rect -> Rect
fitAspect aspect' justify ((ox, oy), (w, h)) = ((ox + shift, oy), wh')
  where
    aspect = w / h
    (wh', shift) = if aspect' < aspect
              then
                  let w' = h * aspect'
                      lat = w - w'
                      shift = case justify of
                          LeftJ   -> -lat
                          CentreJ -> 0
                          RightJ  -> lat
                  in  ((w', h), shift)
              else ((w, w / aspect'), 0)

-- | The resulting rectangle will have the specified aspect ratio and completely
-- covert the specified rectangle.
coverAspect :: Coord -> Rect -> Rect
coverAspect aspect' ((ox, oy), (w, h)) = ((ox, oy), wh')
  where
    aspect = w / h
    wh' = if aspect' > aspect
              then (h * aspect', h)
              else (w, w / aspect')

swap :: (a, a) -> (a, a)
swap (a, b) = (b, a)

-- | True if the point is inside the rectangle
inside :: Point -> Rect -> Bool
inside (x, y) ((ox, oy), (wx, wy)) =
    x >= ox - wx && x <= ox + wx &&
    y >= oy - wy && y <= oy + wy

-- | True if the two rectangles overlap
overlaps :: Rect -> Rect -> Bool
overlaps r0 r1 =
    let (ax0, ay0, ax1, ay1) = rectEdges r0
        (bx0, by0, bx1, by1) = rectEdges r1
    in ax1 > bx0 &&
       ay1 > by0 &&
       ax0 < bx1 &&
       ay0 < by1

rectEdges :: Rect -> (Coord, Coord, Coord, Coord)
rectEdges ((ox, oy), (w, h)) = (x0, y0, x1, y1)
  where
    x0 = ox - w
    y0 = oy - h
    x1 = ox + w
    y1 = oy + h

edgesRect :: (Coord, Coord, Coord, Coord) -> Rect
edgesRect (x0, y0, x1, y1) = ((ox, oy), (w, h))
  where
    ox = (x0 + x1) * 0.5
    oy = (y0 + y1) * 0.5
    w  = (x1 - x0) * 0.5
    h  = (y1 - y0) * 0.5

-- | Limit rectangle size (as distance from centre)
clipRect :: Rect -> Rect -> Rect
clipRect clip r =
    let (cx0, cy0, cx1, cy1) = rectEdges clip
        (x0, y0, x1, y1)     = rectEdges r
    in  edgesRect (cx0 `max` x0, cy0 `max` y0, cx1 `min` x1, cy1 `min` y1)

-- | True if this is a null rectangle
nullRect :: Rect -> Bool
nullRect (_, (0, 0)) = True
nullRect _           = False

-- | The rectangle that contains the two sub-rectangles
appendRect :: Rect -> Rect -> Rect
appendRect r0 r1 =
    if nullRect r0 then r1 else
    if nullRect r1 then r0 else
    edgesRect (appendEdges (rectEdges r0) (rectEdges r1))

appendEdges :: (Coord, Coord, Coord, Coord) -> (Coord, Coord, Coord, Coord) -> (Coord, Coord, Coord, Coord)
appendEdges (ax0, ay0, ax1, ay1) (bx0, by0, bx1, by1) = (x0, y0, x1, y1)
  where
    x0 = min ax0 bx0
    y0 = min ay0 by0
    x1 = max ax1 bx1
    y1 = max ay1 by1

-- | Rotate the point 90 degrees clockwise.
clockwisePoint :: Point -> Point
clockwisePoint (x, y) = (y, -x)

-- | Rotate the point 90 degrees anti-clockwise.
anticlockwisePoint :: Point -> Point
anticlockwisePoint (x, y) = (-y, x)

clockwiseRect :: Rect -> Rect
clockwiseRect (orig, (w, h)) = (clockwisePoint orig, (h, w))

anticlockwiseRect :: Rect -> Rect
anticlockwiseRect (orig, (w, h)) = (anticlockwisePoint orig, (h, w))

