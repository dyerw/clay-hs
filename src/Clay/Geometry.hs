-- | Simple common types for representing shapes and positions
module Clay.Geometry where

data Rect = Rect {rectSize :: Size, rectPosition :: Position}

data Size = Size {sizeWidth :: Int, sizeHeight :: Int}

data Position = Position {positionX :: Int, positionY :: Int}
