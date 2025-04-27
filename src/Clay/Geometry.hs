-- | Simple common types for representing shapes and positions
module Clay.Geometry where

import Clay.Raw.Types

data Rect a = Rect {rectSize :: Size a, rectPosition :: Position a}

fromClayBoundingBox :: ClayBoundingBox -> Rect Float
fromClayBoundingBox (ClayBoundingBox x y w h) =
  Rect
    (Size (realToFrac w) (realToFrac h))
    (Position (realToFrac x) (realToFrac y))

data Size a = Size {sizeWidth :: a, sizeHeight :: a}

data Position a = Position {positionX :: a, positionY :: a}

data Sides a = Sides
  { sidesTop :: a,
    sidesRight :: a,
    sidesLeft :: a,
    sidesBottom :: a
  }

data Corners a = Corners
  { cornersTopLeft :: a,
    cornersTopRight :: a,
    cornersBottomRight :: a,
    cornersBottomLeft :: a
  }

fromClayCornerRadius :: ClayCornerRadius -> Corners Float
fromClayCornerRadius (ClayCornerRadius topLeft topRight bottomLeft bottomRight) =
  Corners
    (realToFrac topLeft)
    (realToFrac topRight)
    (realToFrac bottomLeft)
    (realToFrac bottomRight)
