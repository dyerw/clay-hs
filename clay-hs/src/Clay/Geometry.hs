-- | Simple common types for representing shapes and positions
module Clay.Geometry where

import Clay.Raw.Types

data Rect a = Rect {rectSize :: Size a, rectPosition :: Position a}
  deriving (Eq, Show)

fromClayBoundingBox :: ClayBoundingBox -> Rect Float
fromClayBoundingBox (ClayBoundingBox x y w h) =
  Rect
    (Size (realToFrac w) (realToFrac h))
    (Position (realToFrac x) (realToFrac y))

data Size a = Size {sizeWidth :: a, sizeHeight :: a} deriving (Eq, Show)

toClayDimensions :: (Integral a) => Size a -> ClayDimensions
toClayDimensions (Size w h) = ClayDimensions (fromIntegral w) (fromIntegral h)

data Position a = Position {positionX :: a, positionY :: a} deriving (Eq, Show)

data Sides a = Sides
  { sidesTop :: a,
    sidesRight :: a,
    sidesLeft :: a,
    sidesBottom :: a
  }
  deriving (Eq, Show)

data Corners a = Corners
  { cornersTopLeft :: a,
    cornersTopRight :: a,
    cornersBottomRight :: a,
    cornersBottomLeft :: a
  }
  deriving (Eq, Show)

fromClayCornerRadius :: ClayCornerRadius -> Corners Float
fromClayCornerRadius (ClayCornerRadius topLeft topRight bottomLeft bottomRight) =
  Corners
    (realToFrac topLeft)
    (realToFrac topRight)
    (realToFrac bottomLeft)
    (realToFrac bottomRight)
