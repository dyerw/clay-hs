module Clay.Color where

import Data.Word (Word8)

data Color = Color
  { colorR :: Word8,
    colorG :: Word8,
    colorB :: Word8,
    colorA :: Word8
  }
  deriving (Eq, Show)
