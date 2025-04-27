module Clay.Color where

import Clay.Raw.Types
import Data.Word (Word8)
import Foreign.C

data Color = Color
  { colorR :: Word8,
    colorG :: Word8,
    colorB :: Word8,
    colorA :: Word8
  }
  deriving (Eq, Show)

toClayColor :: Color -> ClayColor
toClayColor (Color r g b a) =
  ClayColor
    (CFloat $ fromIntegral r)
    (CFloat $ fromIntegral g)
    (CFloat $ fromIntegral b)
    (CFloat $ fromIntegral a)

fromClayColor :: ClayColor -> Color
fromClayColor (ClayColor r g b a) =
  Color
    (round r)
    (round g)
    (round b)
    (round a)

red :: Color
red = Color 255 0 0 255

green :: Color
green = Color 0 255 0 255

blue :: Color
blue = Color 0 0 255 255
